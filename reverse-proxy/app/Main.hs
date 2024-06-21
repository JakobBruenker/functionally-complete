{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (catch, SomeException, try)
import Control.Monad (when, void, forever)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.ReverseProxy
import Network.HTTP.Simple qualified as HTTP
import Network.HTTP.Types (status200, movedPermanently301, status401, status503, status502, statusCode)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettingsChain, TLSSettings)
import Options.Applicative
import System.Environment (getEnv)
import System.FilePath (takeDirectory)
import System.FSNotify

data Opts = Opts
  { httpPort :: Int
  , httpsPort :: Int
  }

-- TODO
-- Set up app config dir
-- All the files (API key, active) are there, and are read into MVars
-- The MVars are provided via ReaderT to the app

defaultHttpPort, defaultHttpsPort :: Int
defaultHttpPort = 80
defaultHttpsPort = 443

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

data Env = Blue | Green deriving Eq

serializeEnv :: Env -> String
serializeEnv Blue = "blue"
serializeEnv Green = "green"

parseEnv :: String -> Maybe Env
parseEnv "blue" = Just Blue
parseEnv "green" = Just Green
parseEnv _ = Nothing

port :: Env -> Int
port Blue = 8080
port Green = 8081

main :: IO ()
main = do
  opts <- execParser $ info (optsParser <**> helper)
    ( fullDesc
    <> progDesc "Run the reverse proxy for blue-green deployment"
    <> header "reverse-proxy - A reverse proxy for blue-green deployment" )
  apiKeyFile <- getEnv "API_KEY_FILE"
  apiKey <- BS.pack <$> readFile apiKeyFile
  apiKeyVar <- newMVar apiKey
  runProxy apiKeyVar apiKeyFile (httpPort opts) (httpsPort opts)

runProxy :: MVar BS.ByteString -> FilePath -> Int -> Int -> IO ()
runProxy apiKeyVar apiKeyFile httpPort httpsPort = do
    certPath <- getEnv "SSL_CERT_PATH"
    keyPath <- getEnv "SSL_KEY_PATH"
    manager <- newManager tlsManagerSettings
    putStrLn $ "Starting HTTP redirect server on port " ++ show httpPort
    putStrLn $ "Starting HTTPS reverse proxy on port " ++ show httpsPort
    
    -- Start API key file watcher
    void $ forkIO $ watchApiKeyFile apiKeyVar apiKeyFile
    
    -- Start HTTP server to redirect to HTTPS
    void $ forkIO $ run httpPort redirectApp
    
    -- Start HTTPS server with reverse proxy
    runTLS (tlsSettings certPath keyPath) (setPort httpsPort defaultSettings) (reverseProxyApp apiKeyVar manager)

watchApiKeyFile :: MVar BS.ByteString -> FilePath -> IO ()
watchApiKeyFile apiKeyVar apiKeyFile = withManager $ \mgr -> do
    -- watch for changes in the directory containing the API key file
    void $ watchDir mgr (takeDirectory apiKeyFile) (const True) $ \event ->
        when (eventPath event == apiKeyFile) $ do
            newApiKey <- BS.pack <$> readFile apiKeyFile
            void $ swapMVar apiKeyVar newApiKey
            putStrLn "API key updated"
    -- sleep forever
    forever $ threadDelay 1000000

checkApiKey :: MVar BS.ByteString -> Middleware
checkApiKey apiKeyVar app req respond = do
    apiKey <- readMVar apiKeyVar
    case lookup "X-API-Key" (requestHeaders req) of
        Just key | key == apiKey -> app req respond
        _ -> respond $ responseLBS status401 [("Content-Type", "text/plain")] "Unauthorized"

reverseProxyApp :: MVar BS.ByteString -> Manager -> Application
reverseProxyApp apiKeyVar manager req respond = 
    case pathInfo req of
        ["health"] -> do
            backendHealthy <- checkBackendHealth =<< getActiveEnv
            if backendHealthy
                then respond $ responseLBS status200 [("Content-Type", "text/plain")] "OK"
                else respond $ responseLBS status503 [("Content-Type", "text/plain")] "Service Unavailable"
        ["active"] -> checkApiKey apiKeyVar (getActiveAndRespond respond) req respond
        ["switch"] -> checkApiKey apiKeyVar switchInstance req respond
        _ -> do
            let proxyDest _ = WPRProxyDest . ProxyDest "127.0.0.1" . port <$> getActiveEnv
                handleErrors e _ res = do
                    putStrLn $ "Proxy error: " ++ show e
                    res $ responseLBS status502 
                        [("Content-Type", "text/plain")] 
                        "Unable to process your request at this time. Please try again later."
            (waiProxyTo proxyDest handleErrors manager req respond)

getActiveAndRespond :: (Response -> IO ResponseReceived) -> Application
getActiveAndRespond respond _ _ = do
    env <- getActiveEnv
    respond $ responseLBS status200 [("Content-Type", "text/plain")] (LBS.pack $ serializeEnv env)

getActiveEnv :: IO Env
getActiveEnv = fmap (fromMaybe Blue) $ catch @SomeException (parseEnv . filter (not . isSpace) <$> readFile "/path/to/active_env") (\_ -> pure Nothing)

checkBackendHealth :: Env -> IO Bool
checkBackendHealth env = do
    let request = HTTP.parseRequest_ $ "http://localhost:" ++ show (port env) ++ "/health"
    response <- try $ HTTP.httpLBS request
    case response of
        Left (_ :: HTTP.HttpException) -> return False
        Right res -> return $ statusCode (HTTP.getResponseStatus res) == 200

redirectApp :: Application
redirectApp req respond = do
    let secureHost = "https://" <> BS.unpack (fromMaybe "functionally-complete.com" $ requestHeaderHost req) <> BS.unpack (rawPathInfo req)
    respond $ responseLBS movedPermanently301 [("Location", BS.pack secureHost)] ""

tlsSettings :: FilePath -> FilePath -> TLSSettings
tlsSettings cert key = tlsSettingsChain cert [cert] key

switchInstance :: Application
switchInstance _ respond = do
    currentEnv <- getActiveEnv
    let newEnv = if currentEnv == Blue then Green else Blue
    writeFile "/path/to/active_env" $ serializeEnv newEnv
    respond $ responseLBS status200 [] "Switched instance"
