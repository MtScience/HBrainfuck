module Parser (readExpr) where

import Prelude hiding (read)
import Control.Monad.Except
import Data.Word (Word8)

-- Necessary to build a parser
import Text.Parsec
import Text.Parsec.String (Parser)

-- Defines custom types for data and errors
import Types


-- "Comments" parser combinator. Ignores everything except the allowed symbols
comment :: Parser ()
comment = skipMany $ noneOf "<>[]+-.,"

{-
Increment and decrement parser combinators. Since the are the same except for the sign
and symbols used, the function is parametrized with the symbols to look for and the
resulting value
-}
increment, decrement :: Parser BFOperation
increment = do
    inc <- many1 $ char '+'
    return $ Increment $ fromIntegral $ length inc
decrement = do
    inc <- many1 $ char '-'
    return $ Decrement $ fromIntegral $ length inc

-- Shift parser combinators. Similar to parseIncrement
shiftl, shiftr :: Parser BFOperation
shiftl = char '<' >> return ShiftLeft
shiftr = char '>' >> return ShiftRight

-- IO parser combinators. Similar to the previous four
read, write :: Parser BFOperation
read  = char ',' >> return Read
write = char '.' >> return Print

-- Loop parser combinator. Thanks to the properties of Parsec, it automatically ensures
-- that the brackets are balanced
loop :: Parser BFOperation
loop = do
    char '['
    loop <- code `endBy` comment
    char ']'
    return $ Loop loop

-- Complete parser
code :: Parser BFOperation
code = comment >> choice [increment, decrement, shiftl, shiftr, read, write, loop]

readExpr :: String -> Either BFError [BFOperation]
readExpr input = case parse (code `endBy` comment) "brainfuck" input of
    Left err  -> throwError $ ParseError $ show err
    Right val -> return val
