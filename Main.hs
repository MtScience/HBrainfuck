module Main where

import System.Environment
import Data.Word

import Parser (readExpr)

main :: IO ()
main = getArgs >>= putStrLn . show . readExpr . head
