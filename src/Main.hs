module Main where

import Control.Concurrent (forkFinally)
import Control.Monad      (forever, void)
import Network            (Socket, PortID(..), accept, listenOn, withSocketsDo)
import Text.Printf        (printf)
import System.IO          (Handle(..), BufferMode(..),
                           hGetLine, hClose, hPutStr, hSetNewlineMode, universalNewlineMode, hSetBuffering)

main :: IO ()
main = withSocketsDo $ do
    serverSocket <- startServer
    forever $ acceptClient serverSocket

acceptClient :: Socket -> IO ()
acceptClient serverSocket = do
    (handle, host, port') <- accept serverSocket
    _ <- printf "[Server] Accepted connection from %s\n" host
    void $ forkFinally (handleClient handle) (\_ -> closeClient handle host)

closeClient :: Handle -> String -> IO ()
closeClient handle host = do
    hClose handle
    void $ printf "[Server] Closed connection from %s\n" host

startServer :: IO Socket
startServer = do
    socket <- listenOn (PortNumber (fromIntegral port))
    printf "[Server] Serving on port %d\n" port
    return socket

handleClient :: Handle -> IO ()
handleClient handle = do
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    doPingPong handle

doPingPong :: Handle -> IO ()
doPingPong handle = do
    line <- hGetLine handle
    hPutStr handle line

port :: Int
port = 3500
