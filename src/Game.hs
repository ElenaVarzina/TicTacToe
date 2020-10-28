module Game where

import Data.Maybe

-- Represents a player of the game
data Player = X | O
  deriving (Enum, Eq, Show)

-- Represents contents of a cell on the board
data BoardCell = EmptyCell
               | FilledCell Player
               deriving (Eq, Show)

type Pos = (Int, Int)

data Board = Board { boardSize :: Int
                   , boardGoal :: Int
                   , boardCell :: Pos -> BoardCell
                   }

data Game = Game { board :: Board
                 , toMove :: Player
                 }

-- Game presets

data GamePreset = Game3x3
                | Game5x5
                | Game7x7
                deriving (Eq, Show, Enum)

emptyBoard :: Int -> Int -> Board
emptyBoard size goal = Board { boardSize = size
                             , boardGoal = goal
                             , boardCell = const EmptyCell
                             }

game3x3 :: Game
game3x3 = Game { board = emptyBoard 3 3
               , toMove = X
               }

game5x5 :: Game
game5x5 = Game { board = emptyBoard 5 4
               , toMove = X
               }

game7x7 :: Game
game7x7 = Game { board = emptyBoard 7 4
               , toMove = X
               }

gameFromPreset :: GamePreset -> Game
gameFromPreset Game3x3 = game3x3
gameFromPreset Game5x5 = game5x5
gameFromPreset Game7x7 = game7x7

-- Board utils

isPlayerTheWinner :: Player -> Board -> Bool
isPlayerTheWinner p board = any checkFrom [(x, y) | x <- [0..size-1], y <- [0..size-1]]
  where f = (== (FilledCell p)) . boardCell board
        size = boardSize board
        goal = boardGoal board
        checkFrom pos = checkH pos || checkV pos || checkD pos || checkRD (size - 1 - fst pos, snd pos)
        checkH  (x, y) = all f [(x + d, y) | d <- [0..goal-1] ]
        checkV  (x, y) = all f [(x, y + d) | d <- [0..goal-1] ]
        checkD  (x, y) = all f [(x + d, y + d) | d <- [0..goal-1] ]
        checkRD (x, y) = all f [(x - d, y + d) | d <- [0..goal-1] ]

getBoardWinner :: Board -> Maybe Player
getBoardWinner board
  | isPlayerTheWinner X board = Just X
  | isPlayerTheWinner O board = Just O
  | otherwise = Nothing

getBoardMoves :: Board -> [Pos]
getBoardMoves board = do
  let size = boardSize board
  x <- [0..size-1]
  y <- [0..size-1]
  if boardCell board (x, y) == EmptyCell then return (x, y) else []

setBoardCell :: Pos -> BoardCell -> Board -> Board
setBoardCell pos cell board = board { boardCell = f }
  where g = boardCell board
        f pos' = if pos == pos' then cell else g pos'

-- Game utils

makePlayerMove :: Pos -> Game -> Maybe Game
makePlayerMove pos game
  | pos `notElem` getBoardMoves (board game) = Nothing
  | otherwise = Just $ game { board = setBoardCell pos (FilledCell p) (board game)
                            , toMove = op p
                            }
    where p = toMove game
          op X = O
          op O = X

isGameFinished :: Game -> Bool
isGameFinished game = null (getBoardMoves b) || isJust (getBoardWinner b)
  where b = board game

getGameWinner :: Game -> Maybe Player
getGameWinner = getBoardWinner . board
