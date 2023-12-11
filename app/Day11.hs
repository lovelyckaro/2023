{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad
import Data.List
import SantaLib
import SantaLib.Parsing
import Text.Megaparsec.Char.Lexer qualified as Lex

inputExpanders :: String -> Int -> Int -> ([Int], [Int])
inputExpanders inp rowExpand colExpand = (go 1 ls, go 1 cs)
  where
    ls = lines inp
    cs = transpose ls
    go currLine [] = []
    go currLine (line : lines)
      | all (== '.') line = currLine : go (currLine + rowExpand + 1) lines
      | otherwise = go (currLine + 1) lines

expandInput :: String -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
expandInput inp rowExpand colExpand nodes = do
  let (expandedRows, expandedCols) = inputExpanders inp rowExpand colExpand
  (col, row) <- nodes
  let row' = foldl' (\r expand -> if r >= expand then r + rowExpand else r) row expandedRows
  let col' = foldl' (\c expand -> if c >= expand then c + colExpand else c) col expandedCols
  return (col', row')

pos :: Parser (Int, Int)
pos = do
  p <- getSourcePos
  return (unPos p.sourceColumn, unPos p.sourceLine)

pInp :: Parser [(Int, Int)]
pInp = do
  let graphSpace = void $ many (string "." <|> eol)
  graphSpace
  nodes <- some $ Lex.lexeme graphSpace (pos <* string "#")
  eof
  return nodes

manDist :: (Int, Int) -> (Int, Int) -> Int
manDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

minDistsFrom :: (Int, Int) -> [(Int, Int)] -> [Int]
minDistsFrom start nodes = manDist start <$> nodes

part1 :: [(Int, Int)] -> Int
part1 nodes = (`div` 2) . sum $ do
  node <- nodes
  minDistsFrom node nodes

main :: IO ()
main = do
  inp <- getInput 11
  nodes <- parseIO pInp "day11.input" inp
  let nodes1 = expandInput inp 1 1 nodes
  let nodes2 = expandInput inp (1000000 - 1) (1000000 - 1) nodes
  -- example <- getExample 11 >>= parseIO pInp "day11-example.input"
  putAnswer 11 Part1 (part1 nodes1)
  putAnswer 11 Part2 (part1 nodes2)
