{-# LANGUAGE OverloadedStrings #-}
module Main where

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

optsParser :: Parser Opts
optsParser = Opts
  <$> option auto
      ( long "http-port"
     <> short 'p'
     <> metavar "HTTP_PORT"
     <> value 80
     <> help "Port for HTTP server (default: 80)" )
  <*> option auto
      ( long "https-port"
     <> short 's'
     <> metavar "HTTPS_PORT"
     <> value 443
     <> help "Port for HTTPS server (default: 443)" )

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
            (port, _) <- getActivePortAndEnv
            backendHealthy <- checkBackendHealth port
            if backendHealthy
                then respond $ responseLBS status200 [("Content-Type", "text/plain")] "OK"
                else respond $ responseLBS status503 [("Content-Type", "text/plain")] "Service Unavailable"
        ["active"] -> checkApiKey apiKeyVar (getActiveAndRespond respond) req respond
        ["switch"] -> checkApiKey apiKeyVar switchInstance req respond
        _ -> do
            (port, _) <- getActivePortAndEnv
            let proxyDest _ = return $ WPRProxyDest $ ProxyDest "127.0.0.1" port
            catch 
                (waiProxyTo proxyDest defaultOnExc manager req respond)
                (\(e :: SomeException) -> do
                    putStrLn $ "Error: " ++ show e
                    respond $ responseLBS status502 
                        [("Content-Type", "text/plain")] 
                        "Unable to process your request at this time. Please try again later.")

getActiveAndRespond :: (Response -> IO ResponseReceived) -> Application
getActiveAndRespond respond _ _ = do
    (_, env) <- getActivePortAndEnv
    respond $ responseLBS status200 [("Content-Type", "text/plain")] (LBS.pack env)

getActivePort :: IO Int
getActivePort = do
    content <- catch (readFile "/path/to/active_env") handleError
    return $ if trim content == "blue" then 8080 else 8081
  where
    handleError :: SomeException -> IO String
    handleError _ = return "blue"  -- Default to blue if file read fails
    trim = filter (not . isSpace)

getActiveEnvironment :: IO String
getActiveEnvironment = do
    content <- catch (readFile "/path/to/active_env") handleError
    return $ trim content
  where
    handleError :: SomeException -> IO String
    handleError _ = return "blue"  -- Default to blue if file read fails
    trim = filter (not . isSpace)

getActivePortAndEnv :: IO (Int, String)
getActivePortAndEnv = do
    content <- catch (readFile "/path/to/active_env") handleError
    let env = trim content
    return $ if env == "blue" then (8080, "blue") else (8081, "green")
  where
    handleError :: SomeException -> IO String
    handleError _ = return "blue"  -- Default to blue if file read fails
    trim = filter (not . isSpace)

checkBackendHealth :: Int -> IO Bool
checkBackendHealth port = do
    let request = HTTP.parseRequest_ $ "http://localhost:" ++ show port ++ "/health"
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
    (_, currentEnv) <- getActivePortAndEnv
    let newEnv = if currentEnv == "blue" then "green" else "blue"
    writeFile "/path/to/active_env" newEnv
    respond $ responseLBS status200 [] "Switched instance"
