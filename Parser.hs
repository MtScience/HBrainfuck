module Parser where

-- Defines custom types for data and errors
import Types (BFOperation(..))

-- Necessary to build a parser
import Text.Parsec
import Text.Parsec.String

{-
Increment and decrement parser combinator. Since the are the same except for the sign
and symbols used, the function is parametrized with the symbols to look for and the
resulting value
-}
parseIncrement :: Char -> (Int -> BFOperation) -> Parser BFOperation
parseIncrement sym op = (many1 $ char sym) >>= \inc -> return $ op $ length inc

-- Shift parser combinator. Similar to parseIncrement
parseShift :: Char -> BFOperation -> Parser BFOperation
parseShift sym op = char sym >> return op

-- IO parser combinator. Similar to the previous two
parseIO :: Char -> BFOperation -> Parser BFOperation
parseIO sym op = char sym >> return op

-- Loop parser combinator. Thanks to the properties of Parsec, it automatically ensures
-- that the brackets are balanced
parseLoop :: Parser BFOperation
parseLoop = do
  char '['
  loop <- parseBFcode `manyTill` (char ']')
  return $ Loop loop

-- Comlete parser
parseBFcode :: Parser BFOperation
parseBFcode = parseIncrement '+' Increment
           <|> parseIncrement '-' Decrement
           <|> parseShift '>' ShiftRight
           <|> parseShift '<' ShiftLeft
           <|> parseIO ',' Read
           <|> parseIO '.' Print
           <|> parseLoop

readExpr :: String -> String
readExpr input = case parse (parseBFcode `manyTill` eof) "Not found" input of
  Left err  -> show err
  Right val -> "Found expression: " ++ show val
