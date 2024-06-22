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
import Control.Exception (SomeException, try, handle)
import Control.Monad (void, forever)
import Control.Monad.Reader (ReaderT, runReaderT, ask, liftIO)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.ReverseProxy
import Network.HTTP.Simple qualified as HTTP
import Network.HTTP.Types.Header (hReferer)
import Network.HTTP.Types.Status (ok200, movedPermanently301, unauthorized401, serviceUnavailable503, badGateway502, temporaryRedirect307, statusCode)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettingsChain, TLSSettings)
import System.Directory (createDirectoryIfMissing, getHomeDirectory, canonicalizePath)
import System.Environment (getEnv)
import System.FilePath ((</>), equalFilePath)
import System.FSNotify
import System.IO.Error (isDoesNotExistError)

import Options

-- TODO I'm not sure App Application makes sense, that's double IO. Seems like it might have stale MVar values then?

data AppConfig = AppConfig
  { apiKeyVar :: MVar BS.ByteString
  , portsVar :: MVar Ports
  , manager :: Manager
  , configDir :: FilePath
  }

type App = ReaderT AppConfig IO

data Ports = Ports
  { active   :: Int
  , inactive :: Int
  }

apiKeyFileName, portsFileName :: FilePath
apiKeyFileName = "api_key"
portsFileName = "ports.conf"

main :: IO ()
main = do
  opts <- parseOpts
  configDir <- if "~" `isPrefixOf` opts.configDir
                then (</> drop 2 opts.configDir) <$> getHomeDirectory
                else pure opts.configDir
  createDirectoryIfMissing True configDir
  
  let portsFile = configDir </> portsFileName
  let apiKeyFile = configDir </> apiKeyFileName
  
  apiKey <- readApiKey apiKeyFile
  apiKeyVar <- newMVar apiKey
  
  activePorts <- readPorts portsFile
  portsVar <- newMVar activePorts
  
  manager <- newManager tlsManagerSettings
  
  let appConfig = AppConfig{..}
  
  runProxy appConfig configDir opts.httpPort opts.httpsPort

readApiKey :: FilePath -> IO BS.ByteString
readApiKey file = do
  apiKey <- BS.pack . takeWhile (not . isSpace) . dropWhile isSpace <$> readFile file
  if BS.null apiKey
    then fail "API key file is empty"
    else return apiKey

readPorts :: FilePath -> IO Ports
readPorts file = do
  result <- try $ readFile file
  case result of
    Left e ->
      if isDoesNotExistError e
        then writeDefaultPorts file
        else fail $ "Error reading ports file: " ++ show e
    Right content -> case parsePorts content of
      Just ports -> return ports
      Nothing -> fail "Invalid ports file format"
  where
    writeDefaultPorts f = do
      let defaultPorts = Ports{active = 8080, inactive = 8081}
      writeFile f $ portsToString defaultPorts
      pure defaultPorts

parsePorts :: String -> Maybe Ports
parsePorts content = do
  let pairs = mapMaybe parseLine $ lines content
  active <- lookup "active" pairs
  inactive <- lookup "inactive" pairs
  return $ Ports active inactive

parseLine :: String -> Maybe (String, Int)
parseLine line = case break (== ':') $ dropWhile (== ' ') line of
  (key, ':':value) -> case reads $ dropWhile (== ' ') value of
    [(intValue, "")] -> Just (key, intValue)
    _ -> Nothing
  _ -> Nothing

portsToString :: Ports -> String
portsToString Ports{..} = unlines
  [ "active: " ++ show active
  , "inactive: " ++ show inactive
  ]

runProxy :: AppConfig -> FilePath -> Int -> Int -> IO ()
runProxy appConfig configDir httpPort httpsPort = flip runReaderT appConfig do
    certPath <- liftIO $ getEnv "SSL_CERT_PATH"
    keyPath <- liftIO $ getEnv "SSL_KEY_PATH"
    liftIO . putStrLn $ "Watching config files in " ++ configDir
    liftIO . void $ forkIO $ watchConfigFiles appConfig

    liftIO . putStrLn $ "Starting HTTP redirect server on port " ++ show httpPort
    liftIO . void $ forkIO $ run httpPort redirectApp
    
    app <- reverseProxyApp
    liftIO $ putStrLn $ "Starting HTTPS reverse proxy on port " ++ show httpsPort
    liftIO $ runTLS (tlsSettings certPath keyPath) (setPort httpsPort defaultSettings) app

watchConfigFiles :: AppConfig -> IO ()
watchConfigFiles AppConfig{..} = withManager $ \mgr -> do
    canonicalConfigDir <- canonicalizePath configDir
    let apiKeyFile = canonicalConfigDir </> apiKeyFileName
        portsFile = canonicalConfigDir </> portsFileName
    void $ watchDir mgr configDir (const True) $ \event -> do
        path <- canonicalizePath (eventPath event)
        if | equalFilePath path apiKeyFile -> handle
               do \(e :: SomeException) -> putStrLn $ "Error updating API key: " ++ show e
               do newApiKey <- readApiKey apiKeyFile
                  void $ swapMVar apiKeyVar newApiKey
                  putStrLn "API key updated"
           | equalFilePath path portsFile -> handle
               do \(e :: SomeException) -> putStrLn $ "Error updating ports: " ++ show e
               do newPorts <- readPorts portsFile
                  void $ swapMVar portsVar newPorts
                  putStrLn $ "Ports updated: " <> show newPorts.active <> " (active), " <> show newPorts.inactive <> " (inactive)"
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
    pure \req respond -> runReaderT (proxyApp req respond) appConfig

proxyApp :: Request -> (Response -> IO ResponseReceived) -> App ResponseReceived
proxyApp req respond = do
    AppConfig{..} <- ask
    case pathInfo req of
        ["health"] -> liftIO $ healthCheck portsVar req respond
        ["preview-health"] -> liftIO $ checkApiKey apiKeyVar (healthCheck portsVar) req respond
        ["active"] -> liftIO $ checkApiKey apiKeyVar (getActiveAndRespond portsVar respond) req respond
        ["switch"] -> liftIO $ checkApiKey apiKeyVar (switchPorts configDir portsVar respond) req respond
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

-- TODO Why are we getting respond as a separate argument and then ignoring it the second time
switchPorts :: FilePath -> MVar Ports -> (Response -> IO ResponseReceived) -> Application
switchPorts configDir portsVar respond _ _ = do
    let portsFile = configDir </> portsFileName
    currentPorts <- readMVar portsVar
    let newPorts = Ports { active = currentPorts.inactive, inactive = currentPorts.active }
    writeFile portsFile $ portsToString newPorts
    respond $ responseLBS ok200 [] "Port switch initiated"
