{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (catch, SomeException, try)
import Control.Monad (void, forever, when)
import Control.Monad.Reader (ReaderT, runReaderT, ask, liftIO)
import Control.Monad.Identity
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, isNothing)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.ReverseProxy
import Network.HTTP.Simple qualified as HTTP
import Network.HTTP.Types.Header (hReferer)
import Network.HTTP.Types.Status (ok200, movedPermanently301, unauthorized401, serviceUnavailable503, badGateway502, temporaryRedirect307, statusCode)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettingsChain, TLSSettings)
import Options.Applicative hiding (action)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Environment (getEnv, getProgName)
import System.FilePath ((</>))
import System.FSNotify
import Data.Text (Text)
import Data.Text qualified as T

-- TODO I'm not sure App Application makes sense, that's double IO. Seems like it might have stale MVar values then?

data Opts = Opts
  { httpPort :: Int
  , httpsPort :: Int
  , configDir :: FilePath
  }

data AppConfig = AppConfig
  { apiKeyVar :: MVar BS.ByteString
  , portsVar :: MVar Ports
  , manager :: Manager
  }

type App = ReaderT AppConfig IO

type family HKD f a where
  HKD Identity  a = a
  HKD (Const b) a = b
  HKD f         a = f a

data Ports' f = Ports
  { active   :: HKD f Int
  , inactive :: HKD f Int
  }

type Ports = Ports' Identity
type PortFiles = Ports' (Const FilePath)

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

main :: IO ()
main = do
  progName <- getProgName
  opts <- execParser $ info (optsParser <**> helper)
    ( fullDesc
    <> progDesc "Run the reverse proxy for blue-green deployment"
    <> header progName )
  
  configDir <- if "~" `isPrefixOf` opts.configDir
                then (</> drop 2 opts.configDir) <$> getHomeDirectory
                else pure opts.configDir
  createDirectoryIfMissing True configDir
  
  let apiKeyFile = configDir </> "api_key"
      active = configDir </> "active_port"
      inactive = configDir </> "inactive_port"
  
  apiKey <- readApiKey apiKeyFile
  apiKeyVar <- newMVar apiKey
  
  activePorts <- readPortsFiles Ports{active, inactive}
  portsVar <- newMVar activePorts
  
  manager <- newManager tlsManagerSettings
  
  let appConfig = AppConfig{..}
  
  runProxy appConfig configDir opts.httpPort opts.httpsPort

readApiKey :: FilePath -> IO BS.ByteString
readApiKey file = BS.pack . takeWhile (not . isSpace) . dropWhile isSpace <$> readFile file

readPortsFiles :: PortFiles -> IO Ports
readPortsFiles portFiles = do
  active <- readPortFile portFiles.active 8080
  inactive <- readPortFile portFiles.inactive 8081
  pure Ports{..}

readPortFile :: FilePath -> Int -> IO Int
readPortFile file defaultPort = do
  content <- (Just <$> readFile file) `catch` \(_ :: SomeException) -> pure Nothing
  when (isNothing content) $ writeFile file (show defaultPort)
  pure . read . filter (not . isSpace) $ fromMaybe (show defaultPort) content

runProxy :: AppConfig -> FilePath -> Int -> Int -> IO ()
runProxy appConfig configDir httpPort httpsPort = do
    certPath <- getEnv "SSL_CERT_PATH"
    keyPath <- getEnv "SSL_KEY_PATH"
    putStrLn $ "Starting HTTP redirect server on port " ++ show httpPort
    putStrLn $ "Starting HTTPS reverse proxy on port " ++ show httpsPort
    
    void $ forkIO $ watchConfigFiles configDir appConfig
    void $ forkIO $ run httpPort redirectApp
    
    app <- runReaderT reverseProxyApp appConfig
    runTLS (tlsSettings certPath keyPath) (setPort httpsPort defaultSettings) app

watchConfigFiles :: FilePath -> AppConfig -> IO ()
watchConfigFiles configDir AppConfig{..} = withManager $ \mgr -> do
    let apiKeyFile = configDir </> "api_key"
        active = configDir </> "active_port"
        inactive = configDir </> "inactive_port"

    void $ watchDir mgr configDir (const True) $ \event -> do
        let path = eventPath event
        if | path == apiKeyFile -> do
               newApiKey <- readApiKey apiKeyFile
               void $ swapMVar apiKeyVar newApiKey
               putStrLn "API key updated"
           | path == active || path == inactive -> do
               newPorts <- readPortsFiles Ports{..}
               void $ swapMVar portsVar newPorts
               putStrLn "Ports updated"
           | otherwise -> return ()

    forever $ threadDelay 1000000

checkApiKey :: MVar BS.ByteString -> Middleware
checkApiKey apiKeyVar app req respond = do
    apiKey <- readMVar apiKeyVar
    putStrLn $ "API key: " ++ show apiKey
    putStrLn $ "Request headers: " ++ show (requestHeaders req)
    case lookup "X-API-Key" (requestHeaders req) of
        Just key | key == apiKey -> app req respond
        _ -> respond $ responseLBS unauthorized401 [("Content-Type", "text/plain")] "Unauthorized"

reverseProxyApp :: App Application
reverseProxyApp = do
    appConfig <- ask
    return $ \req respond -> runReaderT (proxyApp req respond) appConfig

proxyApp :: Request -> (Response -> IO ResponseReceived) -> App ResponseReceived
proxyApp req respond = do
    AppConfig{manager, portsVar, apiKeyVar} <- ask
    case pathInfo req of
        ["health"] -> liftIO $ healthCheck portsVar req respond
        ["preview-health"] -> liftIO $ checkApiKey apiKeyVar (healthCheck portsVar) req respond
        ["active"] -> liftIO $ checkApiKey apiKeyVar (getActiveAndRespond portsVar respond) req respond
        ["switch"] -> liftIO $ checkApiKey apiKeyVar (switchPorts portsVar respond) req respond
        "preview" : rest -> liftIO $ checkApiKey apiKeyVar (previewInactiveServer portsVar manager rest) req respond
        _ -> do
            case checkRefererForPreview req of
                Just previewPath -> redirectToPreview previewPath req respond
                Nothing -> do
                    Ports{..} <- liftIO $ readMVar portsVar
                    let proxyDest _ = return $ WPRProxyDest $ ProxyDest "127.0.0.1" active
                    liftIO $ waiProxyTo proxyDest handleErrors manager req respond

healthCheck :: MVar Ports -> Application
healthCheck portsVar _ res = do
    Ports{..} <- readMVar portsVar
    backendHealthy <- checkBackendHealth active
    if backendHealthy
        then res $ responseLBS ok200 [("Content-Type", "text/plain")] "OK"
        else res $ responseLBS serviceUnavailable503 [("Content-Type", "text/plain")] "Service Unavailable"

checkRefererForPreview :: Request -> Maybe BS.ByteString
checkRefererForPreview req = do
    referer <- lookup hReferer (requestHeaders req)
    if "/preview/" `BS.isInfixOf` referer || "/preview" `BS.isSuffixOf` referer
        then Just "/preview"
        else Nothing

redirectToPreview :: BS.ByteString -> Request -> (Response -> IO ResponseReceived) -> ReaderT AppConfig IO ResponseReceived
redirectToPreview previewPrefix req respond = do
    let path = rawPathInfo req
        newLocation = previewPrefix <> path
    liftIO $ respond $ responseLBS temporaryRedirect307
        [ ("Location", newLocation)
        , ("Content-Type", "text/plain")
        ] 
        "Redirecting to preview version"

handleErrors :: SomeException -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleErrors e _ res = liftIO $ do
    putStrLn $ "Proxy error: " ++ show e
    res $ responseLBS badGateway502 
        [("Content-Type", "text/plain")] 
        "Unable to process your request at this time. Please try again later."

previewInactiveServer :: MVar Ports -> Manager -> [Text] -> Application
previewInactiveServer portsVar manager pathSegments req respond = do
      Ports{..} <- liftIO $ readMVar portsVar
      let newPath = BS.intercalate "/" $ map (BS.pack . T.unpack) pathSegments
          proxyReq = req { rawPathInfo = newPath
                         , requestHeaderHost = Just "localhost"
                         }
      let proxyDest _ = return $ WPRProxyDest $ ProxyDest "127.0.0.1" inactive
      waiProxyTo proxyDest handleErrors manager proxyReq respond

getActiveAndRespond :: MVar Ports -> (Response -> IO ResponseReceived) -> Application
getActiveAndRespond portsVar respond _ _ = do
    Ports{..} <- readMVar portsVar
    respond $ responseLBS ok200 [("Content-Type", "text/plain")] (LBS.pack $ show active)

checkBackendHealth :: Int -> IO Bool
checkBackendHealth port = do
    let request = HTTP.parseRequest_ $ "http://localhost:" ++ show port ++ "/health"
    response <- liftIO $ try $ HTTP.httpLbs request
    pure case response of
        Left (_ :: HTTP.HttpException) -> False
        Right res -> statusCode (HTTP.getResponseStatus res) == 200

redirectApp :: Application
redirectApp req respond = do
    let secureHost = "https://" <> BS.unpack (fromMaybe "functionally-complete.com" $ requestHeaderHost req) <> BS.unpack (rawPathInfo req)
    respond $ responseLBS movedPermanently301 [("Location", BS.pack secureHost)] ""

tlsSettings :: FilePath -> FilePath -> TLSSettings
tlsSettings cert key = tlsSettingsChain cert [cert] key

switchPorts :: MVar Ports -> (Response -> IO ResponseReceived) -> Application
switchPorts portsVar respond _ _ = do
    Ports{..} <- readMVar portsVar
    _ <- swapMVar portsVar Ports{active = inactive, inactive = active}
    respond $ responseLBS ok200 [] "Switched ports"
