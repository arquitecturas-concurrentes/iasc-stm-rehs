module Main where

import qualified Rehs.Server as RehsServer

main :: IO ()
main = RehsServer.startServer port

port :: Int
port = 3500

