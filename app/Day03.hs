{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad
import Data.Char (isDigit)
import Data.List
import Data.Map qualified as M
import Data.Vector qualified as V
import SantaLib
import SantaLib.Parsing hiding (lexeme)
import Text.Megaparsec.Char.Lexer qualified as Lex

data SchemaToken = Number SourcePos Int | Symbol SourcePos Char
  deriving (Show)

schemaSpace :: Parser ()
schemaSpace = void $ many (char '.' <|> char '\n')

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme schemaSpace

pSchemaToken :: Parser SchemaToken
pSchemaToken = lexeme (pNum <|> pSymbol)
  where
    pNum = Number <$> getSourcePos <*> decimal
    pSymbol = Symbol <$> getSourcePos <*> satisfy isSymbol

isSymbol :: Char -> Bool
isSymbol c = c /= '.' && not (isDigit c)

pSchema :: Parser [SchemaToken]
pSchema = do
  optional schemaSpace
  some pSchemaToken

neighbors :: SchemaToken -> [SourcePos]
neighbors = \case
  Number pos n ->
    let width = length (show n)
        col = unPos pos.sourceColumn
        line = unPos pos.sourceLine
     in [ pos {sourceColumn = mkPos c, sourceLine = mkPos l}
          | c <- [col - 1 .. col + width],
            c >= 1,
            l <- [line - 1 .. line + 1],
            l >= 1
        ]
  Symbol pos n ->
    let col = unPos pos.sourceColumn
        line = unPos pos.sourceLine
     in [ pos {sourceColumn = mkPos c, sourceLine = mkPos l}
          | c <- [col - 1 .. col + 1],
            c >= 1,
            l <- [line - 1 .. line + 1],
            l >= 1
        ]

symbols :: [SchemaToken] -> M.Map SourcePos Char
symbols tokens = M.fromList [(pos, char) | Symbol pos char <- tokens]

partNumbers :: [SchemaToken] -> [Int]
partNumbers schema = do
  let syms = symbols schema
  num@(Number pos n) <- schema
  neighb <- neighbors num
  guard (neighb `M.member` syms)
  return n

gearNumbers :: [SchemaToken] -> M.Map SourcePos [Int]
gearNumbers schema = M.fromListWith (<>) $ do
  let gears = M.filter (== '*') $ symbols schema
  num@(Number pos n) <- schema
  neighb <- neighbors num
  guard (neighb `M.member` gears)
  return (neighb, [n])

part1 :: [SchemaToken] -> Int
part1 = sum . partNumbers

part2 :: [SchemaToken] -> Int
part2 = sum . M.map product . M.filter ((== 2) . length) . gearNumbers

main :: IO ()
main = do
  inp <- getInput 3 >>= parseIO pSchema "day3.input"
  putAnswer 3 Part1 (part1 inp)
  putAnswer 3 Part2 (part2 inp)
