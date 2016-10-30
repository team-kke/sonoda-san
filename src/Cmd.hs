module Cmd
  ( getConfigPath
  ) where

import Options.Applicative

data Cmd = Cmd { configPath :: FilePath
               }

parser :: Parser Cmd
parser = Cmd <$> strOption ( long "config-path"
                          <> short 'c'
                          <> metavar "FILE"
                          <> help "Path to configuration YAML file"
                           )

cmd :: IO Cmd
cmd = execParser $
  info (helper <*> parser) ( fullDesc
                          <> header sonoda
                           )
  where
    sonoda = "sonoda - A LINE bot likely having something to do with Sonoda Umi"

getConfigPath :: IO FilePath
getConfigPath = configPath <$> cmd
