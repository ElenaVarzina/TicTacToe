module Server (serverMain) where

import Game
import Protocol
import Data.Serialize
import Data.Maybe
import System.Random
import Network.WebSockets hiding (Close)
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Map.Strict as M

data Client = Client { game :: Maybe Game
                     , cmds :: [Cmd]
                     }

makeRandomMove :: Client -> IO Client
makeRandomMove client = do
  let g = fromJust $ game client
  let moves = getBoardMoves (board g)
  midx <- randomRIO (0, length moves - 1)
  let game' = fromJust $ makePlayerMove (moves !! midx) g
  return $ client { game = Just game'
                  , cmds = (cmds client) <> [MakeMove (moves !! midx)]
                  }

respond :: Client -> Cmd -> IO Client
respond client cmd = do
  case cmd of
    NewGame preset _ -> do
      t <- randomIO
      let game = gameFromPreset preset
      if t then return $ client { game = Just game
                                , cmds = [NewGame preset X]
                                }
           else do
             let moves = getBoardMoves $ board game
             midx <- randomRIO (0, length moves - 1)
             let game' = fromJust $ makePlayerMove (moves !! midx) game
             return $ client { game = Just game'
                             , cmds = [NewGame preset O, MakeMove (moves !! midx)]
                             }
    MakeMove pos -> 
      case game client of
        Just g -> do
          case makePlayerMove pos g of
            Just game' -> if not (isGameFinished game') then makeRandomMove $ client { game = Just game' , cmds = [cmd] }
                                                        else return $ client { game = Just game', cmds = [cmd] }
            Nothing -> return client
        Nothing -> return client

server :: ServerApp
server pending = do
  conn <- acceptRequest pending
  client <- newMVar $ Client Nothing []

  flip finally (return ()) $ forever $ do
    bdata <- receiveData conn
    case decode bdata of
      Left err -> sendClose conn (encode Close)
      Right Close -> sendClose conn (encode Close)
      Right cmd -> do
        modifyMVar_ client $ \c -> do
          client' <- respond c cmd
          sendBinaryDatas conn $ map encode $ cmds client'
          return $ client' { cmds = [] }

serverMain :: IO ()
serverMain = do
  let host = "0.0.0.0"
  let port = 8888
  putStrLn $ "Starting server on " <> host <> ":" <> show port
  runServer host port server
