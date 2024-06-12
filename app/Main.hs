{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Network.Wai.Middleware.Static
import Network.HTTP.Types (ok200, movedPermanently301, notFound404)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import System.FilePath ((<.>), hasExtension)
import System.Directory (doesFileExist)
import Control.Concurrent (forkIO)
import Data.Maybe (fromMaybe)
import System.Environment (getEnv)

portHTTP :: Int
portHTTP = 80

portHTTPS :: Int
portHTTPS = 443

main :: IO ()
main = do
    certPath <- getEnv "SSL_CERT_PATH"
    keyPath <- getEnv "SSL_KEY_PATH"
    putStrLn $ "Starting HTTP server on http://localhost:" <> show portHTTP
    putStrLn $ "Starting HTTPS server on https://localhost:" <> show portHTTPS
    -- HTTP to HTTPS redirect server
    _ <- forkIO $ run portHTTP redirectApp
    -- HTTPS server
    runTLS (tlsSettings certPath keyPath) (setPort portHTTPS defaultSettings) app

app :: Application
app req respond = do
    let path = dropWhile (== '/') . BS.unpack $ rawPathInfo req
    let fileToServe | null path = "index.html"
                    | hasExtension path = path
                    | otherwise = path <.> "html"
    staticApp fileToServe req respond

staticApp :: FilePath -> Application
staticApp path req respond = do
    case tryPolicy (addBase "resources" >-> noDots >-> isNotAbsolute) path of
        Nothing -> notFound req respond
        Just filePath -> do
          fileExists <- doesFileExist filePath
          if fileExists
            then respond $ responseFile ok200 [("Content-Type", getMimeType filePath)] filePath Nothing
            else notFound req respond

-- Fallback response if the file is not found
notFound :: Application
-- Need to use responseLBS since responseFile without explicit FilePart handling ignores the status code
notFound _ respond = respond . responseLBS notFound404 [("Content-Type", "text/html")] =<< LBS.readFile "resources/404.html"

-- Redirect HTTP to HTTPS
redirectApp :: Application
redirectApp req respond = do
    let secureHost = "https://" <> BS.unpack (fromMaybe "functionally-complete.com" $ requestHeaderHost req) <> BS.unpack (rawPathInfo req)
    respond $ responseLBS movedPermanently301 [("Location", BS.pack secureHost)] ""
