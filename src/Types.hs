module Types where

import Data.Word (Word8)


data BFOperation = Print
                 | Read
                 | ShiftRight
                 | ShiftLeft
                 | Increment Word8
                 | Decrement Word8
                 | Loop [BFOperation]
		 -- The following can't be directly input by the user
                 | Reset

newtype BFError = ParseError String

instance Show BFError where
    show (ParseError err) = "Parse error:\n" ++ err


data Tape = Tape [Word8] Word8 [Word8]
