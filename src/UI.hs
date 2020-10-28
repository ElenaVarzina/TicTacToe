{-# LANGUAGE OverloadedStrings #-}

module UI (uiMain) where

import Game
import Protocol
import Client
import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe
import Data.Vector (fromList)
import Brick
import Brick.BChan
import Brick.Widgets.List
import Brick.Widgets.Border
import Brick.Widgets.Center
import Graphics.Vty

data AppState = AppState { game   :: Maybe Game
                         , preset :: List String GamePreset
                         , sel    :: Pos
                         , me     :: Player
                         , chan   :: BChan Cmd
                         }

initialAppState :: BChan Cmd -> AppState
initialAppState chan =
  AppState { game = Nothing
           , preset = list "preset" (fromList [Game3x3, Game5x5, Game7x7]) 1
           , sel = (0, 0)
           , me = X
           , chan = chan
           }

type MyApp   = App AppState Cmd String
type MyEvent = BrickEvent String Cmd

app :: MyApp
app = App { appDraw         = draw
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const $ attrMap defAttr
            [ ("me", fg red `withStyle` bold)
            , ("notMe", fg blue)
            ]
          }

sendCmd :: AppState -> Cmd -> EventM String (Next AppState)
sendCmd s cmd = do
  liftIO $ writeBChan (chan s) cmd
  continue s

handleEventGame :: AppState -> MyEvent -> EventM String (Next AppState)
handleEventGame s (VtyEvent (EvKey key [])) = case key of
    KLeft  -> moved (-1) 0
    KRight -> moved 1 0
    KUp    -> moved 0 (-1)
    KDown  -> moved 0 1
    KEsc   -> halt s
    KEnter -> if me s == toMove (fromJust $ game s) then sendCmd s $ MakeMove (sel s)
                                                    else continue s
    _      -> continue s
  where moved dy dx = let x' = (x + dx) `mod` size
                          y' = (y + dy) `mod` size
                      in continue $ s { sel = (x', y') }
        size = boardSize $ board $ fromJust $ game s
        x = fst $ sel s
        y = snd $ sel s
handleEventGame s _ = continue s

handleEventGameOver :: AppState -> MyEvent -> EventM String (Next AppState)
handleEventGameOver s (VtyEvent (EvKey (KChar ' ') [])) = continue $ s { game = Nothing }
handleEventGameOver s _ = continue s

handleEventSetup :: AppState -> MyEvent -> EventM String (Next AppState)
handleEventSetup s (VtyEvent (EvKey KEnter [])) = sendCmd s $ NewGame p X
  where p = snd $ fromJust $ listSelectedElement $ preset s
handleEventSetup s (VtyEvent e) = do
  l <- handleListEvent e (preset s)
  continue $ s { preset = l }
handleEventSetup s _ = continue s

handleEvent :: AppState -> MyEvent -> EventM String (Next AppState)
handleEvent s (AppEvent Close) = halt s
handleEvent s (VtyEvent (EvKey KEsc [])) = halt s
handleEvent s (AppEvent (NewGame preset me)) = continue $ s { game = Just $ gameFromPreset preset, me = me }
handleEvent s (AppEvent (MakeMove pos)) = continue $ s { game = game s >>= makePlayerMove pos }
handleEvent s e = case game s of
                  Just g | isGameFinished g -> handleEventGameOver s e
                         | otherwise -> handleEventGame s e
                  Nothing -> handleEventSetup s e

drawGame :: AppState -> [Widget String]
drawGame s = [ center $ border $ vBox rows ]
  where rows      = map (hBox . cols) idx
        cols r    = map (cell r) idx
        cell r c  = padAll 1 $ act r c (withAttr (getAttr r c) $ str $ p2str $ bc r c)
        size      = boardSize b
        idx       = [0..size-1]
        b         = board $ fromJust $ game s
        p2str (FilledCell X) = "X"
        p2str (FilledCell O) = "O"
        p2str EmptyCell = " "
        bc r c = boardCell b (r, c)
        getAttr r c = case bc r c of
                        EmptyCell -> "empty"
                        FilledCell t -> if t == me s then "me" else "notMe"
        act r c t = if (r, c) == sel s then str "[" <+> t <+> str "]"
                                       else str " " <+> t <+> str " "

drawGameOver :: AppState -> [Widget String]
drawGameOver s = [ center $ str (gameResultString (fromJust $ game s)) <=> str "" <=> str "Press SPACE to continue, ESC to quit" ]
  where gameResultString g = case getBoardWinner (board g) of
                               Just X -> "X is the winner!"
                               Just O -> "O is the winner!"
                               Nothing -> "Tie!"

drawSetup :: AppState -> [Widget String]
drawSetup s = [ center $ renderList r True (preset s) ]
  where r sel el = if sel then str $ "> " <> (show el) <> " <"
                          else str $ "  " <> (show el) <> "  "

draw :: AppState -> [Widget String]
draw s = case game s of
             Just g | isGameFinished g -> drawGameOver s
                    | otherwise -> drawGame s
             Nothing -> drawSetup s


uiMain :: IO ()
uiMain = do
  putStrLn "Enter host to connect to (e.g. 127.0.0.1):"
  host <- getLine
  upstream <- newBChan 10
  downstream <- newBChan 10
  forkFinally (client upstream downstream host 8888) $ \result ->
    case result of
      Left err -> writeBChan downstream Close >> throwIO err
      Right _ -> return ()
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (pure downstream) app $ initialAppState upstream
