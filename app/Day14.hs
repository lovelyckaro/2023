{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad
import Control.Monad.State
import Data.Map (Map, (!?))
import Data.Map qualified as M
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as S
import SantaLib
import SantaLib.Parsing hiding (State)
import Text.Megaparsec.Char.Lexer qualified as Lex
import Prelude hiding (cycle)

data Node = Round (Int, Int) | Square (Int, Int)
  deriving (Show, Eq)

nodePos :: Node -> (Int, Int)
nodePos (Round p) = p
nodePos (Square p) = p

instance Ord Node where
  compare n1 n2 = case comparing nodePos n1 n2 of
    EQ -> case (n1, n2) of
      (Round _, Square _) -> LT
      (Square _, Round _) -> GT
      _ -> EQ
    x -> x

pos :: Parser (Int, Int)
pos = do
  p <- getSourcePos
  return (unPos p.sourceColumn, unPos p.sourceLine)

pInp :: Parser (Set Node)
pInp = do
  let graphSpace = void $ many (string "." <|> eol)
  graphSpace
  nodes <- some $ Lex.lexeme graphSpace (Square <$> pos <* string "#" <|> Round <$> pos <* string "O")
  eof
  return $ S.fromList nodes

isSquare :: Node -> Bool
isSquare (Square _) = True
isSquare _ = False

isRound :: Node -> Bool
isRound = not . isSquare

rollNorth :: Int -> Set Node -> Set Node
rollNorth minRow startingNodes = S.foldl' rollOne (S.filter isSquare startingNodes) (S.filter isRound startingNodes)
  where
    rollOne :: Set Node -> Node -> Set Node
    rollOne nodes nodeToRoll = case S.lookupLT nodeToRoll nodes of
      Just lt ->
        let (col, row) = nodePos lt
            (oldCol, oldRow) = nodePos nodeToRoll
         in if col == oldCol
              then S.insert (Round (col, row + 1)) nodes
              else S.insert (Round (oldCol, minRow)) nodes
      Nothing ->
        let (oldCol, oldRow) = nodePos nodeToRoll
         in S.insert (Round (oldCol, minRow)) nodes

draw :: Set Node -> String
draw nodes =
  unlines
    [ [ case ((col, row) `S.member` squares, (col, row) `S.member` rounds) of
          (True, True) -> '💀'
          (True, False) -> '#'
          (False, True) -> 'O'
          (False, False) -> '.'
        | col <- [minCol .. maxCol]
      ]
      | row <- [minRow .. maxRow]
    ]
  where
    minRow = S.findMin $ S.map (snd . nodePos) nodes
    maxRow = S.findMax $ S.map (snd . nodePos) nodes
    minCol = S.findMin $ S.map (fst . nodePos) nodes
    maxCol = S.findMax $ S.map (fst . nodePos) nodes
    squares = S.map nodePos $ S.filter isSquare nodes
    rounds = S.map nodePos $ S.filter isRound nodes

rotate :: Set Node -> Set Node
rotate = S.map rotateNode
  where
    rotateNode (Round (col, row)) = Round (-row, col)
    rotateNode (Square (col, row)) = Square (-row, col)

cycle :: Set Node -> Set Node
cycle nodes = rotate . rollNorth minEast . rotate . rollNorth minSouth . rotate . rollNorth minWest . rotate . rollNorth minNorth $ nodes
  where
    maxRow = S.findMax $ S.map (snd . nodePos) nodes
    minNorth = 1
    minEast = -maxRow
    minSouth = -maxRow
    minWest = 1

cycleManyTimes :: Int -> Int -> Set Node -> State (Map (Set Node) Int) (Set Node)
cycleManyTimes maxCycles currCycle nodes | currCycle == maxCycles = return nodes
cycleManyTimes maxCycles currCycle nodes = do
  gets (!? nodes) >>= \case
    Just previousCycle -> do
      let cycleDiff = currCycle - previousCycle
      let untilDoneDiff = maxCycles - currCycle
      let skipAmount = cycleDiff * (untilDoneDiff `div` cycleDiff)
      let next = cycle nodes
      cycleManyTimes maxCycles (currCycle + skipAmount + 1) next
    Nothing -> do
      let next = cycle nodes
      modify (M.insert nodes currCycle)
      cycleManyTimes maxCycles (currCycle + 1) next

score :: Set Node -> Int
score = sum . map (\(Round (col, row)) -> 101 - row) . S.toList . S.filter isRound

part1 :: Set Node -> Int
part1 = score . rollNorth 1

part2 :: Set Node -> Int
part2 = score . (`evalState` M.empty) . cycleManyTimes 1000000000 0

main :: IO ()
main = do
  inp <- getInput 14 >>= parseIO pInp "day14.input"
  -- example <- getExample 14 >>= parseIO pInp "day14-example.input"
  putAnswer 14 Part1 (part1 inp)
  putAnswer 14 Part2 (part2 inp)
