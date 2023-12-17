{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Algorithm.Search
import Control.Monad (guard)
import Data.List (singleton)
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import SantaLib

pInp :: String -> Vector (Vector Int)
pInp = V.fromList . map (V.fromList . map (read . singleton)) . lines

data Direction = North | East | South | West
  deriving (Show, Eq, Ord, Enum)

data State = State {row, col :: !Int, currDir :: !Direction, lastTurned :: !Int}
  deriving (Show, Eq, Ord)

lookupCost :: Vector (Vector Int) -> State -> State -> Int
lookupCost costs from to = costs ! to.row ! to.col

uTurn :: Direction -> Direction -> Bool
uTurn North South = True
uTurn South North = True
uTurn East West = True
uTurn West East = True
uTurn _ _ = False

step :: State -> Direction -> State
step from dir =
  let (row, col) = case dir of
        North -> (from.row - 1, from.col)
        East -> (from.row, from.col + 1)
        South -> (from.row + 1, from.col)
        West -> (from.row, from.col - 1)
      lastTurned =
        if dir == from.currDir
          then from.lastTurned + 1
          else 0
   in State {row = row, col = col, currDir = dir, lastTurned = lastTurned}

inBounds :: Vector (Vector Int) -> State -> Bool
inBounds costs p =
  let maxRow = V.length costs
      maxCol = V.length (costs ! 0)
   in p.col >= 0 && p.col < maxCol && p.row >= 0 && p.row < maxRow

neighbors :: Vector (Vector Int) -> State -> [State]
neighbors costs from = do
  dir <- [North .. West]
  guard $ not (uTurn from.currDir dir)
  let next = step from dir
  guard $ inBounds costs next
  guard $ next.lastTurned < 3
  return next

neighbors2 :: Vector (Vector Int) -> State -> [State]
neighbors2 costs from = do
  dir <- [North .. West]
  guard $ not (uTurn from.currDir dir)
  guard $ from.currDir == dir || from.lastTurned >= 3
  let next = step from dir
  guard $ inBounds costs next
  guard $ next.lastTurned < 10
  return next

startingState :: State
startingState = State 0 0 East 0

part1 :: Vector (Vector Int) -> Int
part1 costs = maybe 0 fst $ dijkstra (neighbors costs) (lookupCost costs) lowerRight startingState
  where
    maxCol = V.length (costs ! 0)
    maxRow = V.length costs
    lowerRight p = p.row == maxRow - 1 && p.col == maxCol - 1

part2 :: Vector (Vector Int) -> Int
part2 costs = maybe 0 fst $ dijkstra (neighbors2 costs) (lookupCost costs) lowerRight startingState
  where
    maxCol = V.length (costs ! 0)
    maxRow = V.length costs
    lowerRight p = p.row == maxRow - 1 && p.col == maxCol - 1 && p.lastTurned >= 3

main :: IO ()
main = do
  inp <- pInp <$> getInput 17
  -- example <- getExample 17 >>= parseIO pInp "day17-example.input"
  putAnswer 17 Part1 (part1 inp)
  putAnswer 17 Part2 (part2 inp)
