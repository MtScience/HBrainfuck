module Types where

import Data.Word (Word8)

-- All the brainfuck operations. Increment and Decrement are parametrized
-- to allow for optimization
data BFOperation = Print
                 | Read
                 | ShiftRight
                 | ShiftLeft
                 | Increment Word8
                 | Decrement Word8
                 | Loop [BFOperation]
                 | Reset

{-
A type for returning errors. Although, brainfuck is too simple to have many errors:
- it has no functions to allow for incorrect number of arguments,
- it has no types to allow for type mismatches,
- it has no functions to allow for call errors...
So, basically, the only error I can think of is unbalanced brackets, which is
a parsing error. Therefore...
-}
newtype BFError = ParseError String

instance Show BFError where
    show (ParseError err) = "Parse error:\n" ++ err

{-
Brainfuck uses the concept of a linear tape of byte-sized cells of length
at least 30,000, with a pointer referencing one of those cells. In C that
would be expressed as

	char tape[30000] = {0};
	int current = 0;

In Python that would be a list (although, I don't know of an obvoious way to limit the
cell size in Python:

	tape: list[int] = [0] * 30_000
	current: int = 0

Haskell, though, doesn't allow mutability and list random access is slow. Fortunately,
in brainfuck you can't shift more than one cell at a time, so I will use a zipper:
a three-field structure, containing (possibly empty) lists of Word8's in the first and
third fields, and a Word8 in the second. Field 1 represents all the cells before current
in reverse order, field 2 represent the current cell and field 3 represents the cells
after current. E.g., if at some point the tape looks like this (storing a string "Haskell"):

    072 097 115 107 101 108 108 000 000 ...
                ^^^

it would be represented as:

	Tape [115, 97, 72] 107 [101, 108, 108]

Initial value for the structure is:

	Tape [] 0 []

This structure allows for fast switching between cells: it's just taking the head of the left list,
setting it to the current element and appending the previous current element to the head of the
right list, which is all very fast in Haskell. E.g.:

	shiftRight, shiftLeft :: Tape -> Tape
	shiftRight (Tape ls c (r:rs) = Tape (c:ls) r rs
	shiftLeft  (Tape (l:ls) c rs = Tape ls l (c:rs)

Also, it allows me to spare memory, using Haskell's laziness: it's possible to only store the lists
up to the last non-zero element.
-}
data Tape = Tape [Word8] Word8 [Word8]
