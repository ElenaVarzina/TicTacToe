module Main where

import UI
import Server
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> uiMain
    ["--listen"] -> serverMain
