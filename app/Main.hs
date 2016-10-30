module Main where

import Config (getPort)
import Network.Wai.Handler.Warp (run)
import Server (app)

main :: IO ()
main = do
  port <- getPort
  putStrLn $ "http://0.0.0.0:" ++ show port
  run port app
