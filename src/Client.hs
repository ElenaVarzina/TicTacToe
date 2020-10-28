module Client (client) where

import Game
import Protocol
import Network.WebSockets
import Network.WebSockets.Client
import Control.Concurrent
import Control.Monad
import Data.Serialize
import Brick.BChan

clientUpstream :: BChan Cmd -> Connection -> IO ()
clientUpstream upstream conn = forever $ do
  cmd <- readBChan upstream
  sendBinaryData conn (encode cmd)

clientDownstream :: BChan Cmd -> Connection -> IO ()
clientDownstream downstream conn = forever $ do
  (Right cmd) <- decode <$> receiveData conn
  writeBChan downstream cmd

client :: BChan Cmd -> BChan Cmd -> String -> Int -> IO ()
client upstream downstream host port = runClient host port "/" $ \conn -> do
  forkIO $ clientUpstream upstream conn
  clientDownstream downstream conn
