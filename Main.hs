module Main where

import Control.Exception (catch)
import Control.Monad.State
import System.Environment
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.IO.Error

import Evaluator
import Parser
import Types

main :: IO ()
main = do
  args <- getArgs
  case args of
    []         -> runREPL initTape
    [filename] -> runScript filename
    _          -> printUsage

printUsage :: IO ()
printUsage = do
  hPutStrLn stderr  "Incorrect number of arguments. Usage:"
  hPutStrLn stderr  "  brainfuck"
  hPutStrLn stderr  "to run in interactive mode or"
  hPutStrLn stderr  "  brainfuck <script>"
  hPutStrLn stderr  "to run a script." 
  exitFailure

runScript :: FilePath -> IO ()
runScript path = do
  program <- readFile path `catch` handler path
  _ <- execute program initTape
  return ()
  where
    handler :: FilePath -> IOError -> IO String
    handler _ err = do
      hPutStrLn stderr $ "Unable to open file '" ++ path ++ "': " ++ ioeGetErrorString err
      exitFailure

-- Doesn't automatically move to newline if the input shippet outputs something
runREPL :: Tape -> IO ()
runREPL st = do
  putStr "brainfuck:: " >> hFlush stdout
  program <- getLine
  if program == "@"
  then
    return ()
  else do
    st' <- execute program st
    runREPL st'
  
execute :: String -> Tape -> IO Tape
execute program st = do
  let parsed = readExpr program
  case parsed of
    Right code -> run code st
    Left err   -> (hPutStrLn stderr $ show err) >> exitFailure

initTape = Tape [] 0 []
