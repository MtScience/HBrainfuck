module Parser (readExpr) where

import Prelude hiding (read)
import Control.Monad.Except
import Data.Word (Word8)

import Text.Parsec
import Text.Parsec.String (Parser)

import Types


stripComments :: String -> String
stripComments = filter (`elem` "<>[]+-.,")

increment, decrement :: Parser BFOperation
increment = Increment . fromIntegral . length <$> many1 (char '+')
decrement = Decrement . fromIntegral . length <$> many1 (char '-')

shiftl, shiftr :: Parser BFOperation
shiftl = ShiftLeft <$ char '<'
shiftr = ShiftRight <$ char '>'

read, write :: Parser BFOperation
read  = Read <$ char ','
write = Print <$ char '.'

loop :: Parser BFOperation
loop = Loop <$> between (char '[') (char ']') code

code :: Parser [BFOperation]
code = many $ choice [increment, decrement, shiftl, shiftr, read, write, loop]

readExpr :: String -> Either BFError [BFOperation]
readExpr input = case parse code "brainfuck" $ stripComments input of
    Left err  -> throwError $ ParseError $ show err
    Right val -> return $ optimize val

optimize :: [BFOperation] -> [BFOperation]
optimize []                           = []
optimize (Increment inc : Decrement dec : ops)
    | inc > dec = optimize $ Increment (inc - dec) : ops
    | inc < dec = optimize $ Decrement (dec - inc) : ops
    | otherwise = ops
optimize (Decrement dec : Increment inc : ops)
    | inc > dec = optimize $ Increment (inc - dec) : ops
    | inc < dec = optimize $ Decrement (dec - inc) : ops
    | otherwise = ops
optimize ((Loop [Increment 1]) : ops) = optimize $ Reset : ops
optimize ((Loop [Decrement 1]) : ops) = optimize $ Reset : ops
optimize (Loop [] : ops)              = optimize ops
optimize (Loop loop : ops)            = Loop (optimize loop) : optimize ops
optimize (op:ops)                     = op : optimize ops
