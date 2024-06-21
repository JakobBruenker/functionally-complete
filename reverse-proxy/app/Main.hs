{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (catch, SomeException, try)
import Control.Monad (when, void, forever)
import Control.Monad.Reader (ReaderT, runReaderT, ask, liftIO)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.ReverseProxy
import Network.HTTP.Simple qualified as HTTP
import Network.HTTP.Types.Status (ok200, movedPermanently301, unauthorized401, serviceUnavailable503, badGateway502, statusCode)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettingsChain, TLSSettings)
import Options.Applicative
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Environment (getEnv)
import System.FilePath ((</>), takeDirectory)
import System.FSNotify

data Opts = Opts
  { httpPort :: Int
  , httpsPort :: Int
  , configDir :: FilePath
  }

data AppConfig = AppConfig
  { apiKeyVar :: MVar BS.ByteString
  , activeEnvVar :: MVar Env
  , manager :: Manager
  }

type App = ReaderT AppConfig IO

defaultHttpPort, defaultHttpsPort :: Int
defaultHttpPort = 80
defaultHttpsPort = 443

defaultConfigDir :: FilePath
defaultConfigDir = "~/.config/functionally-complete"

optsParser :: Parser Opts
optsParser = Opts
  <$> option auto
      ( long "http-port"
     <> short 'p'
     <> metavar "HTTP_PORT"
     <> value defaultHttpPort
     <> help ("Port for HTTP server (default: " <> show defaultHttpPort <> ")") )
  <*> option auto
      ( long "https-port"
     <> short 's'
     <> metavar "HTTPS_PORT"
     <> value defaultHttpsPort
     <> help ("Port for HTTPS server (default: " <> show defaultHttpsPort <> ")") )
  <*> strOption
      ( long "config-dir"
     <> short 'c'
     <> metavar "CONFIG_DIR"
     <> value defaultConfigDir
     <> help ("Configuration directory (default: " <> defaultConfigDir <> ")") )

data Env = Blue | Green deriving (Eq, Read, Show)

serializeEnv :: Env -> String
serializeEnv = show

parseEnv :: String -> Maybe Env
parseEnv s = case reads s of
  [(env, "")] -> Just env
  _ -> Nothing

port :: Env -> Int
port Blue = 8080
port Green = 8081

main :: IO ()
main = do
  opts <- execParser $ info (optsParser <**> helper)
    ( fullDesc
    <> progDesc "Run the reverse proxy for blue-green deployment"
    <> header "reverse-proxy - A reverse proxy for blue-green deployment" )
  
  configDir <- if "~" `isPrefixOf` configDir opts
                then (</> drop 2 (configDir opts)) <$> getHomeDirectory
                else return $ configDir opts
  createDirectoryIfMissing True configDir
  
  let apiKeyFile = configDir </> "api_key"
      activeEnvFile = configDir </> "active_env"
  
  apiKey <- BS.pack <$> readFile apiKeyFile
  apiKeyVar <- newMVar apiKey
  
  activeEnv <- readEnvFile activeEnvFile
  activeEnvVar <- newMVar activeEnv
  
  manager <- newManager tlsManagerSettings
  
  let appConfig = AppConfig {..}
  
  runProxy appConfig apiKeyFile activeEnvFile (httpPort opts) (httpsPort opts)

readEnvFile :: FilePath -> IO Env
readEnvFile file = do
  content <- readFile file `catch` \(_ :: SomeException) -> return "Blue"
  return $ fromMaybe Blue (parseEnv $ filter (not . isSpace) content)

runProxy :: AppConfig -> FilePath -> FilePath -> Int -> Int -> IO ()
runProxy appConfig apiKeyFile activeEnvFile httpPort httpsPort = do
    certPath <- getEnv "SSL_CERT_PATH"
    keyPath <- getEnv "SSL_KEY_PATH"
    putStrLn $ "Starting HTTP redirect server on port " ++ show httpPort
    putStrLn $ "Starting HTTPS reverse proxy on port " ++ show httpsPort
    
    void $ forkIO $ watchApiKeyFile (apiKeyVar appConfig) apiKeyFile
    void $ forkIO $ watchActiveEnvFile (activeEnvVar appConfig) activeEnvFile
    void $ forkIO $ run httpPort redirectApp
    
    app <- runReaderT reverseProxyApp appConfig
    runTLS (tlsSettings certPath keyPath) (setPort httpsPort defaultSettings) app

watchApiKeyFile :: MVar BS.ByteString -> FilePath -> IO ()
watchApiKeyFile apiKeyVar apiKeyFile = withManager $ \mgr -> do
    void $ watchDir mgr (takeDirectory apiKeyFile) (const True) $ \event ->
        when (eventPath event == apiKeyFile) $ do
            newApiKey <- BS.pack <$> readFile apiKeyFile
            void $ swapMVar apiKeyVar newApiKey
            putStrLn "API key updated"
    forever $ threadDelay 1000000

watchActiveEnvFile :: MVar Env -> FilePath -> IO ()
watchActiveEnvFile activeEnvVar activeEnvFile = withManager $ \mgr -> do
    void $ watchDir mgr (takeDirectory activeEnvFile) (const True) $ \event ->
        when (eventPath event == activeEnvFile) $ do
            newEnv <- readEnvFile activeEnvFile
            void $ swapMVar activeEnvVar newEnv
            putStrLn "Active environment updated"
    forever $ threadDelay 1000000

checkApiKey :: MVar BS.ByteString -> Middleware
checkApiKey apiKeyVar app req respond = do
    apiKey <- readMVar apiKeyVar
    case lookup "X-API-Key" (requestHeaders req) of
        Just key | key == apiKey -> app req respond
        _ -> respond $ responseLBS unauthorized401 [("Content-Type", "text/plain")] "Unauthorized"

reverseProxyApp :: App Application
reverseProxyApp = do
    appConfig <- ask
    return $ \req respond -> runReaderT (proxyApp req respond) appConfig

proxyApp :: Request -> (Response -> IO ResponseReceived) -> App ResponseReceived
proxyApp req respond = do
    AppConfig{..} <- ask
    case pathInfo req of
        ["health"] -> do
            backendHealthy <- checkBackendHealth =<< getActiveEnv
            liftIO $ if backendHealthy
                then respond $ responseLBS ok200 [("Content-Type", "text/plain")] "OK"
                else respond $ responseLBS serviceUnavailable503 [("Content-Type", "text/plain")] "Service Unavailable"
        ["active"] -> liftIO $ checkApiKey apiKeyVar (getActiveAndRespond activeEnvVar respond) req respond
        ["switch"] -> liftIO $ checkApiKey apiKeyVar (switchInstance activeEnvVar respond) req respond
        _ -> do
            activeEnv <- getActiveEnv
            let proxyDest _ = return $ WPRProxyDest $ ProxyDest "127.0.0.1" (port activeEnv)
                handleErrors e _ res = liftIO $ do
                    putStrLn $ "Proxy error: " ++ show e
                    res $ responseLBS badGateway502 
                        [("Content-Type", "text/plain")] 
                        "Unable to process your request at this time. Please try again later."
            liftIO $ waiProxyTo proxyDest handleErrors manager req respond

getActiveAndRespond :: MVar Env -> (Response -> IO ResponseReceived) -> Application
getActiveAndRespond activeEnvVar respond _ _ = do
    env <- readMVar activeEnvVar
    respond $ responseLBS ok200 [("Content-Type", "text/plain")] (LBS.pack $ serializeEnv env)

getActiveEnv :: App Env
getActiveEnv = do
    AppConfig{..} <- ask
    liftIO $ readMVar activeEnvVar

checkBackendHealth :: Env -> App Bool
checkBackendHealth env = do
    let request = HTTP.parseRequest_ $ "http://localhost:" ++ show (port env) ++ "/health"
    response <- liftIO $ try $ HTTP.httpLbs request
    return $ case response of
        Left (_ :: HTTP.HttpException) -> False
        Right res -> statusCode (HTTP.getResponseStatus res) == 200

redirectApp :: Application
redirectApp req respond = do
    let secureHost = "https://" <> BS.unpack (fromMaybe "functionally-complete.com" $ requestHeaderHost req) <> BS.unpack (rawPathInfo req)
    respond $ responseLBS movedPermanently301 [("Location", BS.pack secureHost)] ""

tlsSettings :: FilePath -> FilePath -> TLSSettings
tlsSettings cert key = tlsSettingsChain cert [cert] key

switchInstance :: MVar Env -> (Response -> IO ResponseReceived) -> Application
switchInstance activeEnvVar respond _ _ = do
    currentEnv <- readMVar activeEnvVar
    let newEnv = if currentEnv == Blue then Green else Blue
    _ <- swapMVar activeEnvVar newEnv
    respond $ responseLBS ok200 [] "Switched instance"
