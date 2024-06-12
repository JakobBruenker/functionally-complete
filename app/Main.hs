{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Network.Wai.Middleware.Static
import Network.HTTP.Types (status200, status301, status404)
import qualified Data.ByteString.Char8 as BS
import System.FilePath ((<.>), takeExtension)
import System.Directory (doesFileExist)
import Control.Concurrent (forkIO)
import Data.Maybe (fromMaybe)
import System.Environment (getEnv, getArgs)

portHTTP :: Int
portHTTP = 80

portHTTPS :: Int
portHTTPS = 443

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            certPath <- getEnv "SSL_CERT_PATH"
            keyPath <- getEnv "SSL_KEY_PATH"
            putStrLn $ "Starting HTTP server on http://localhost:" <> show portHTTP
            putStrLn $ "Starting HTTPS server on https://localhost:" <> show portHTTPS
            -- HTTP to HTTPS redirect server
            _ <- forkIO $ run portHTTP redirectApp
            -- HTTPS server
            runTLS (tlsSettings certPath keyPath) (setPort portHTTPS defaultSettings) app
        ["--http"] -> do
            putStrLn $ "Starting HTTP server on http://localhost:" <> show portHTTP
            run portHTTP app
        _ -> putStrLn "Invalid arguments. Usage: functionally-complete [--http]"

app :: Application
app req respond = do
    let path = BS.unpack $ rawPathInfo req
    let fileToServe = if null path || path == "/" 
                      then "index.html"
                      else if takeExtension path == ".html"
                           then dropWhile (== '/') path
                           else dropWhile (== '/') path <.> "html"
    staticApp fileToServe req respond

staticApp :: FilePath -> Application
staticApp path req respond = do
    case tryPolicy (addBase "resources") path of
        Nothing -> notFound req respond
        Just filePath -> do
          fileExists <- doesFileExist filePath
          if fileExists
            then respond $ responseFile status200 [("Content-Type", "text/html")] filePath Nothing
            else notFound req respond

-- Fallback response if the file is not found
notFound :: Application
notFound _ respond = respond $ responseFile status404 [("Content-Type", "text/html")] "resources/404.html" Nothing

-- Redirect HTTP to HTTPS
redirectApp :: Application
redirectApp req respond = do
    let secureHost = "https://" <> BS.unpack (fromMaybe "functionally-complete.com" $ requestHeaderHost req) <> BS.unpack (rawPathInfo req)
    respond $ responseLBS status301 [("Location", BS.pack secureHost)] ""
