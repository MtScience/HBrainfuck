module Main where

import Control.Exception (catch)
import System.Environment
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, hPrint, hPutStrLn, stderr, stdout, isEOF)
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
    _ <- execute program initTape exitFailure
    return ()
    where
        handler :: FilePath -> IOError -> IO String
        handler _ err = do
            hPutStrLn stderr $ "Unable to open file '" ++ path ++ "': " ++ ioeGetErrorString err
            exitFailure

runREPL :: Tape -> IO ()
runREPL st = do
    putStr "brainfuck:: "
    hFlush stdout
    eof <- isEOF
    if eof
    then putStrLn "" >> exitSuccess
    else do
        program <- getLine
        st' <- execute program st $ return st
        runREPL st'
  
execute :: String -> Tape -> IO Tape -> IO Tape
execute program st exitAction = do
    let parsed = readExpr program
    case parsed of
        Right code -> run code st
        Left err   -> hPrint stderr err >> exitAction

initTape :: Tape
initTape = Tape [] 0 []
