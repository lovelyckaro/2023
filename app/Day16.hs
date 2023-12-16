{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad
import Data.Containers.ListUtils (nubOrd)
import Data.Map (Map, (!?))
import Data.Map qualified as M
import SantaLib
import SantaLib.Parsing
import Text.Megaparsec.Char.Lexer qualified as Lex

data Mirror = Horizontal | Vertical | Forward | Backward
  deriving (Show, Eq, Ord)

type Graph = Map (Int, Int) Mirror

data Direction = North | East | South | West
  deriving (Show, Eq, Ord)

step :: (Int, Int) -> Direction -> (Int, Int)
step (col, row) North = (col, row - 1)
step (col, row) East = (col + 1, row)
step (col, row) South = (col, row + 1)
step (col, row) West = (col - 1, row)

outOfBounds :: Int -> Int -> (Int, Int) -> Bool
outOfBounds maxCol maxRow (col, row) =
  not $
    col >= 1
      && col <= maxCol
      && row >= 1
      && row <= maxRow

neighbors :: Int -> Int -> Graph -> ((Int, Int), Direction) -> [((Int, Int), Direction)]
neighbors maxCol maxRow graph (point, dir) | outOfBounds maxCol maxRow (step point dir) = []
neighbors _ _ graph (point, dir) =
  let next = step point dir
   in case graph !? next of
        Nothing -> [(next, dir)]
        Just mirror -> case (dir, mirror) of
          (North, Horizontal) -> [(next, East), (next, West)]
          (North, Vertical) -> [(next, North)]
          (North, Forward) -> [(next, East)]
          (North, Backward) -> [(next, West)]
          (East, Horizontal) -> [(next, East)]
          (East, Vertical) -> [(next, North), (next, South)]
          (East, Forward) -> [(next, North)]
          (East, Backward) -> [(next, South)]
          (South, Horizontal) -> [(next, East), (next, West)]
          (South, Vertical) -> [(next, South)]
          (South, Forward) -> [(next, West)]
          (South, Backward) -> [(next, East)]
          (West, Horizontal) -> [(next, West)]
          (West, Vertical) -> [(next, North), (next, South)]
          (West, Forward) -> [(next, South)]
          (West, Backward) -> [(next, North)]

startingPoint :: ((Int, Int), Direction)
startingPoint = ((0, 1), East)

pos :: Parser (Int, Int)
pos = do
  p <- getSourcePos
  return (unPos p.sourceColumn, unPos p.sourceLine)

pInp :: Parser Graph
pInp = do
  let graphSpace = void $ many (string "." <|> eol)
  graphSpace
  nodes <- some $ Lex.lexeme graphSpace $ do
    p <- pos
    mirror <- choice [Horizontal <$ char '-', Vertical <$ char '|', Forward <$ char '/', Backward <$ char '\\']
    return (p, mirror)
  eof
  return $ M.fromList nodes

bounds :: Graph -> (Int, Int)
bounds g = (maxCol, maxRow)
  where
    maxCol = maximum . map fst . M.keys $ g
    maxRow = maximum . map snd . M.keys $ g

part1 :: Graph -> Int
part1 g = pred . length $ nubOrd $ map fst $ connected (neighbors maxCol maxRow g) startingPoint
  where
    (maxCol, maxRow) = bounds g

part2 :: Map (Int, Int) Mirror -> Int
part2 g = maximum $ map (pred . length . nubOrd . map fst . connected (neighbors maxCol maxRow g)) possibleStarts
  where
    (maxCol, maxRow) = bounds g
    possibleStarts = top <> right <> bottom <> left
    top = [((col, 0), South) | col <- [1 .. maxCol]]
    right = [((maxCol + 1, row), West) | row <- [1 .. maxRow]]
    bottom = [((col, maxRow + 1), North) | col <- [1 .. maxCol]]
    left = [((0, row), East) | row <- [1 .. maxRow]]

main :: IO ()
main = do
  inp <- getInput 16 >>= parseIO pInp "day16.input"
  -- example <- getExample 16 >>= parseIO pInp "day16-example.input"
  putAnswer 16 Part1 (part1 inp)
  putAnswer 16 Part2 (part2 inp)
