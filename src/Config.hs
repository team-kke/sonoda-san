{-# LANGUAGE DeriveGeneric #-}

module Config
  ( getPort
  ) where

import Cmd (getConfigPath)
import Data.Yaml (FromJSON, decodeFile)
import GHC.Generics
import System.Exit (exitFailure)

type Port = Int -- in Network.Wai.Handler.Warp

data Config = Config { port :: Port
                     } deriving Generic

instance FromJSON Config

config :: IO Config
config = do
  config <- getConfigPath >>= decodeFile
  case config of
    Just x -> return x
    Nothing -> putStrLn "invalid config" >> exitFailure

getPort :: IO Port
getPort = port <$> config
