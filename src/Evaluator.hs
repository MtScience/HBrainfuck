module Evaluator (run) where

import Control.Monad.State
import Data.Char (chr, ord)
import Data.Word (Word8)
import System.IO (isEOF)

import Types


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

evaluate :: BFOperation -> StateT Tape IO ()
evaluate (Increment inc) = modify $ increment inc
evaluate (Decrement dec) = modify $ decrement dec
evaluate ShiftLeft       = modify shiftl
evaluate ShiftRight      = modify shiftr
evaluate Reset           = modify $ overwrite 0
evaluate Put = do
    Tape _ val _ <- get
    liftIO $ putChar $ chr $ fromIntegral val
    return ()
evaluate Get = do
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
