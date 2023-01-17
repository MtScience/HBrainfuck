module Parser where

import Text.Parsec
import Text.Parsec.String

data BFOperation = Print
                 | Read
                 | ShiftRight
                 | ShiftLeft
                 | Increment Int
                 | Decrement Int
                 | Loop [BFOperation]
                 deriving Show

parseIncrement :: Char -> (Int -> BFOperation) -> Parser BFOperation
parseIncrement sym op = (many1 $ char sym) >>= \inc -> return $ op $ length inc

parseShift :: Char -> BFOperation -> Parser BFOperation
parseShift sym op = char sym >> return op

parseRead :: Parser BFOperation
parseRead = char ',' >> return Read

parsePrint :: Parser BFOperation
parsePrint = char '.' >> return Print


parseLoop :: Parser BFOperation
parseLoop = do
  char '['
  loop <- parseBFcode `manyTill` (char ']')
  return $ Loop loop


parseBFcode :: Parser BFOperation
parseBFcode = parseIncrement '+' Increment
           <|> parseIncrement '-' Decrement
           <|> parseShift '>' ShiftRight
           <|> parseShift '<' ShiftLeft
           <|> parseRead
           <|> parsePrint
           <|> parseLoop

readExpr :: String -> String
readExpr input = case parse (parseBFcode `manyTill` eof) "Not found" input of
  Left err  -> show err
  Right val -> "Found expression: " ++ show val
