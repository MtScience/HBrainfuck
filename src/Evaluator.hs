module Evaluator (run) where

import Control.Monad.State
import Data.Char (chr, ord)
import Data.Word (Word8)
import System.IO (isEOF)

import Types

{-
First, some helper functions to work with the tape. Placed here instead of
the Types module because these hae to do with actual execution.

The functions "increment" and "decrement", obviously, increment and decrement
the value of the current cell.

The functions "shiftl" and "shiftr" move the pointer to the left or right,
respectively. By using lists in both left and right fields of the Tape structure
I can make the Tape practically infinite in both directions. Of course, portable
brainfuck programs, shouldn't assume the tape has _any_fields to the left, and it
is possible to make it an error, but since it is allowed in the specifications,
easier to implement and allows for greater flexibility, I'll make the tape infinite.

The function "overwrite" stores a value in the current cell, dismissing whatever
was in there before.
-}
increment, decrement :: Word8 -> Tape -> Tape
increment inc (Tape ls cur rs) = Tape ls (cur + inc) rs
decrement dec (Tape ls cur rs) = Tape ls (cur - dec) rs

overwrite :: Word8 -> Tape -> Tape
overwrite val (Tape ls _ rs) = Tape ls val rs

shiftl, shiftr :: Tape -> Tape
shiftl (Tape [] cur rs)     = Tape [] 0 (cur:rs)
shiftl (Tape (l:ls) cur rs) = Tape ls l (cur:rs)

shiftr (Tape ls cur [])     = Tape (cur:ls) 0 []
shiftr (Tape ls cur (r:rs)) = Tape (cur:ls) r rs

-- Evaluator function
evaluate :: BFOperation -> StateT Tape IO ()
evaluate (Increment inc) = modify (increment inc)
evaluate (Decrement dec) = modify (decrement dec)
evaluate ShiftLeft       = modify shiftl
evaluate ShiftRight      = modify shiftr
evaluate Print           = do
    Tape _ val _ <- get
    liftIO $ putChar $ chr $ fromIntegral val
    return ()
evaluate Read            = do
    eof <- liftIO isEOF
    if eof
    then return ()
    else do
        val <- fromIntegral . ord <$> liftIO getChar
        modify (overwrite val)
evaluate loop@(Loop ops) = do
    Tape _ cur _ <- get
    unless (cur == 0) (mapM_ evaluate ops >> evaluate loop)

run :: [BFOperation] -> Tape -> IO Tape
run = execStateT . mapM_ evaluate
