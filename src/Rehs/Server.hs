------------------------------------------------------------
-- Server.
-- This modules declares a server that can accept connections
-- Notice that all functions in this module return STM's
------------------------------------------------------------

module Rehs.Server where

import Control.Concurrent (forkFinally)
import Control.Monad      (forever, void)
import Network            (Socket, PortID(..), accept, listenOn, withSocketsDo)
import Text.Printf        (printf)
import System.IO          (Handle(..), BufferMode(..),
                           hGetLine, hClose, hPutStrLn, hSetNewlineMode, universalNewlineMode, hSetBuffering)

import Rehs (Schema)
import Rehs.IO (newSchemaIO, performAndReadTransactionIO)
import Rehs.Commands (parseTransactionLine)

startServer :: Int -> IO ()
startServer port = withSocketsDo $ do
    schema <- newSchemaIO

    serverSocket <- listenServerPort port
    forever $ acceptClient serverSocket schema

acceptClient :: Socket -> Schema ->  IO ()
acceptClient serverSocket schema = do
    (handle, host, port') <- accept serverSocket
    _ <- printf "[Server] Accepted connection from %s\n" host
    void $ forkFinally (handleClient handle schema) (\_ -> closeClient handle host)

closeClient :: Handle -> String -> IO ()
closeClient handle host = do
    hClose handle
    void $ printf "[Server] Closed connection from %s\n" host

listenServerPort :: Int -> IO Socket
listenServerPort port = do
    socket <- listenOn (PortNumber (fromIntegral port))
    printf "[Server] Serving on port %d\n" port
    return socket

handleClient :: Handle -> Schema -> IO ()
handleClient handle schema = do
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    forever $ handleCommands handle schema

handleCommands :: Handle -> Schema -> IO ()
handleCommands handle schema = do
    line <- hGetLine handle

    let transaction = parseTransactionLine line

    updatedSlot <- performAndReadTransactionIO transaction schema
    hPutStrLn handle updatedSlot

