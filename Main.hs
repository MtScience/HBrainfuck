module Main where

import Control.Monad.State
import System.Environment

import Evaluator (evaluate)
import Parser (readExpr)
import Types

main :: IO ()
main = do
  args <- getArgs
  let ops = readExpr $ head args
  evalStateT (mapM_ evaluate ops) (Tape [] 0 [])
