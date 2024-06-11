{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Static
import Network.HTTP.Types (status200, status404)
import qualified Data.ByteString.Char8 as BS
import System.FilePath ((<.>), takeExtension)
import System.Directory (doesFileExist)

main :: IO ()
main = do
    putStrLn "Starting server on http://localhost:8080"
    run 8080 app

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
