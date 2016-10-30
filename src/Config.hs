module Config
  ( getPort
  ) where

type Port = Int -- in Network.Wai.Handler.Warp

getPort :: IO Port
getPort = return 8080 -- FIXME
