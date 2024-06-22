{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Static
import Network.HTTP.Types (ok200, notFound404)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import System.FilePath ((<.>), hasExtension)
import System.Directory (doesFileExist)
import System.Environment (getProgName)
import Options.Applicative

data Opts = Opts
  { port :: Int
  }

optsParser :: Parser Opts
optsParser = Opts
  <$> option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> help "Port to run the server on" )

main :: IO ()
main = do
  progName <- getProgName
  opts <- execParser $ info (optsParser <**> helper)
    ( fullDesc
    <> progDesc "Run the file serving HTTP server"
    <> header progName )
  runServer (port opts)

runServer :: Int -> IO ()
runServer port = do
    putStrLn $ "Starting HTTP server on https://localhost:" <> show port
    run port app

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

notFound :: Application
notFound _ respond = respond . responseLBS notFound404 [("Content-Type", "text/html")] =<< LBS.readFile "resources/404.html"
