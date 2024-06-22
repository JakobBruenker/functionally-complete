{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Prelude hiding (log)

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (SomeException, try, handle)
import Control.Monad (void, forever, (<=<))
import Control.Monad.Reader (MonadIO, ReaderT, runReaderT, ask, asks, liftIO)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void, vacuous)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.ReverseProxy
import Network.HTTP.Simple qualified as HTTP
import Network.HTTP.Types.Header (hReferer)
import Network.HTTP.Types.Status (ok200, movedPermanently301, unauthorized401, serviceUnavailable503, badGateway502, temporaryRedirect307, statusCode)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
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
type Application' = Request -> (Response -> App ResponseReceived) -> App ResponseReceived
type Middleware' = Application' -> Application'

data Ports = Ports
  { active   :: Int
  , inactive :: Int
  }

log :: MonadIO m => String -> m ()
log = liftIO . putStrLn

domainName :: BS.ByteString
domainName = "functionally-complete.com"

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
  
  runApp AppConfig{..} $ runProxy opts.httpPort opts.httpsPort

runApp :: AppConfig -> App a -> IO a
runApp = flip runReaderT

runApplication' :: AppConfig -> Application' -> Application
runApplication' config app req res = runReaderT (app req (liftIO . res)) config

liftApplication :: Application -> Application'
liftApplication app req res = do
    appConfig <- ask
    liftIO $ app req (runApp appConfig . res)

getConfig :: (AppConfig -> MVar a) -> App a
getConfig = liftIO . readMVar <=< asks

readApiKey :: FilePath -> IO BS.ByteString
readApiKey file = do
  apiKey <- BS.pack . takeWhile (not . isSpace) . dropWhile isSpace <$> readFile file
  if BS.null apiKey
    then fail "API key file is empty"
    else pure apiKey

readPorts :: FilePath -> IO Ports
readPorts file = do
  result <- try $ readFile file
  case result of
    Left e ->
      if isDoesNotExistError e
        then writeDefaultPorts file
        else fail $ "Error reading ports file: " ++ show e
    Right content -> case parsePorts content of
      Just ports -> pure ports
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
  pure $ Ports active inactive

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

plainTextHeader :: HTTP.Header
plainTextHeader = ("Content-Type", "text/plain")

runProxy :: Int -> Int -> App ()
runProxy httpPort httpsPort = do
    certPath <- liftIO $ getEnv "SSL_CERT_PATH"
    keyPath <- liftIO $ getEnv "SSL_KEY_PATH"
    log $ "Starting HTTP redirect server on port " ++ show httpPort
    _ <- liftIO . forkIO $
      run httpPort redirectApp
    
    log $ "Starting HTTPS reverse proxy on port " ++ show httpsPort
    appConfig <- ask
    _ <- liftIO . forkIO $
      runTLS (tlsSettings certPath keyPath) (setPort httpsPort defaultSettings) (runApplication' appConfig reverseProxyApp)

    log . ("Watching config files in " <>) =<< asks (.configDir)
    vacuous watchConfigFiles

watchConfigFiles :: App Void
watchConfigFiles = do
    AppConfig{..} <- ask
    liftIO $ withManager $ \mgr -> do
        canonicalConfigDir <- canonicalizePath configDir
        let apiKeyFile = canonicalConfigDir </> apiKeyFileName
            portsFile = canonicalConfigDir </> portsFileName
        void $ watchDir mgr configDir (const True) $ \event -> do
            path <- canonicalizePath (eventPath event)
            if | equalFilePath path apiKeyFile -> handle
                   do \(e :: SomeException) -> log $ "Error updating API key: " ++ show e
                   do newApiKey <- readApiKey apiKeyFile
                      void $ swapMVar apiKeyVar newApiKey
                      log "API key updated"
               | equalFilePath path portsFile -> handle
                   do \(e :: SomeException) -> log $ "Error updating ports: " ++ show e
                   do newPorts <- readPorts portsFile
                      void $ swapMVar portsVar newPorts
                      log $ "Ports updated: " <> show newPorts.active <> " (active), " <> show newPorts.inactive <> " (inactive)"
               | otherwise -> pure ()
        forever $ threadDelay 1000000

checkApiKey :: Middleware'
checkApiKey app req respond = do
    apiKey <- getConfig (.apiKeyVar)
    case lookup "X-API-Key" (requestHeaders req) of
        Just key | key == apiKey -> app req respond
        _ -> respond $ responseLBS unauthorized401 [plainTextHeader] "Unauthorized"

reverseProxyApp :: Application'
reverseProxyApp req respond = do
    app <- case pathInfo req of
        ["health"] -> pure healthCheck
        ["preview-health"] -> pure $ checkApiKey healthCheck
        ["active"] -> pure $ checkApiKey getActiveAndRespond
        ["switch"] -> pure $ checkApiKey switchPorts
        "preview" : rest -> pure . checkApiKey $ previewInactiveServer rest
        _ -> do
            case checkRefererForPreview req of
                Just previewPath -> pure $ redirectToPreview previewPath
                Nothing -> do
                    Ports{..} <- getConfig (.portsVar)
                    let proxyDest _ = pure . WPRProxyDest $ ProxyDest "127.0.0.1" active
                    pure . liftApplication . waiProxyTo proxyDest handleErrors =<< asks (.manager)
    app req respond

healthCheck :: Application'
healthCheck _ res = do
    Ports{active} <- getConfig (.portsVar)
    backendHealthy <- liftIO $ checkBackendHealth active
    let (status, msg) | backendHealthy = (ok200, "OK")
                      | otherwise = (serviceUnavailable503, "Service Unavailable")
    res $ responseLBS status [plainTextHeader] msg

-- TODO really, this should only trigger if the prefix of the referer also matches
checkRefererForPreview :: Request -> Maybe BS.ByteString
checkRefererForPreview req = do
    referer <- lookup hReferer (requestHeaders req)
    if "/preview/" `BS.isInfixOf` referer || "/preview" `BS.isSuffixOf` referer
        then Just "/preview"
        else Nothing

redirectToPreview :: BS.ByteString -> Application'
redirectToPreview previewPrefix req respond = do
    let path = rawPathInfo req
        newLocation = previewPrefix <> path
    respond $ responseLBS temporaryRedirect307
        [ ("Location", newLocation)
        , plainTextHeader
        ] 
        "Redirecting to preview version"

handleErrors :: SomeException -> Application
handleErrors e _ res = liftIO $ do
    log $ "Proxy error: " ++ show e
    res $ responseLBS badGateway502 
        [plainTextHeader] 
        "Unable to process your request at this time. Please try again later."

previewInactiveServer :: [Text] -> Application'
previewInactiveServer pathSegments req respond = do
      Ports{inactive} <- getConfig (.portsVar)
      let newPath = BS.intercalate "/" $ map (BS.pack . T.unpack) pathSegments
          proxyReq = req { rawPathInfo = newPath
                         , requestHeaderHost = Just "localhost"
                         }
      let proxyDest _ = pure $ WPRProxyDest $ ProxyDest "127.0.0.1" inactive
      manager <- asks (.manager)
      liftApplication (waiProxyTo proxyDest handleErrors manager) proxyReq respond

getActiveAndRespond :: Application'
getActiveAndRespond _ respond = do
    Ports{active} <- liftIO . readMVar =<< asks (.portsVar)
    respond $ responseLBS ok200 [plainTextHeader] (LBS.pack $ show active)

checkBackendHealth :: Int -> IO Bool
checkBackendHealth port = do
    let request = HTTP.parseRequest_ $ "http://localhost:" ++ show port ++ "/health"
    response <- try $ HTTP.httpLbs request
    pure case response of
        Left (_ :: HTTP.HttpException) -> False
        Right res -> statusCode (HTTP.getResponseStatus res) == 200

redirectApp :: Application
redirectApp req respond = do
    let secureHost = "https://" <> BS.unpack (fromMaybe domainName $ requestHeaderHost req) <> BS.unpack (rawPathInfo req)
    respond $ responseLBS movedPermanently301 [("Location", BS.pack secureHost)] ""

switchPorts :: Application'
switchPorts _ respond = do
    AppConfig{..} <- ask
    let portsFile = configDir </> portsFileName
    currentPorts <- liftIO $ readMVar portsVar
    let newPorts = Ports { active = currentPorts.inactive, inactive = currentPorts.active }
    liftIO . writeFile portsFile $ portsToString newPorts
    respond $ responseLBS ok200 [] "Port switch initiated"
