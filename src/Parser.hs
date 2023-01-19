module Parser (readExpr) where

import Prelude hiding (read)
import Control.Monad.Except
import Data.Word (Word8)

-- Necessary to build a parser
import Text.Parsec
import Text.Parsec.String (Parser)

-- Defines custom types for data and errors
import Types


stripComments :: String -> String
stripComments = filter (`elem` "<>[]+-.,")

-- Increment and decrement parser combinators
increment, decrement :: Parser BFOperation
increment = (Increment . fromIntegral . length) <$> (many1 $ char '+')
decrement = (Decrement . fromIntegral . length) <$> (many1 $ char '-')

-- Shift parser combinators. Similar to parseIncrement
shiftl, shiftr :: Parser BFOperation
shiftl = const ShiftLeft <$> char '<'
shiftr = const ShiftRight <$> char '>'

-- IO parser combinators. Similar to the previous four
read, write :: Parser BFOperation
read  = const Read <$> char ','
write = const Print <$> char '.'

-- Loop parser combinator. Thanks to the properties of Parsec, it automatically ensures
-- that the brackets are balanced
loop :: Parser BFOperation
loop = Loop <$> between (char '[') (char ']') code

-- Complete parser
code :: Parser [BFOperation]
code = many $ choice [increment, decrement, shiftl, shiftr, read, write, loop]

readExpr :: String -> Either BFError [BFOperation]
readExpr input = case parse code "brainfuck" $ stripComments input of
    Left err  -> throwError $ ParseError $ show err
    Right val -> return val
