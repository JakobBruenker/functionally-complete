module Options (Opts(..), parseOpts) where

import Options.Applicative
import System.Environment (getProgName)

data Opts = Opts
  { httpPort :: Int
  , httpsPort :: Int
  , configDir :: FilePath
  }

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
    
parseOpts :: IO Opts
parseOpts = do
  progName <- getProgName
  execParser $ info (optsParser <**> helper)
    ( fullDesc
    <> progDesc "Run the reverse proxy for blue-green deployment"
    <> header progName )
