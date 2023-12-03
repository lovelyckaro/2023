module Main where

import Control.Monad
import Data.Char (isDigit)
import Data.List
import Data.Map qualified as M
import Data.Vector qualified as V
import SantaLib
import SantaLib.Parsing

type Schematic = [String]

pInp :: String -> Schematic
pInp = lines

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (row, col) = [(r, c) | r <- [row - 1, row, row + 1], c <- [col - 1, col, col + 1]]

points :: Schematic -> [(Int, Int)]
points schema = do
  row <- [0 .. length schema - 1]
  col <- [0 .. length (head schema) - 1]
  pure (row, col)

symbols :: Schematic -> M.Map (Int, Int) Char
symbols schema =
  M.fromList $
    [ ((r, c), symbol)
      | (r, c) <- points schema,
        let symbol = schema !! r !! c,
        isSymbol symbol
    ]
  where
    isSymbol c = c /= '.' && not (isDigit c)

numbers :: Schematic -> [(String, [(Int, Int)])]
numbers schema = filter (/= ("", [])) $ go schema (0, 0) ("", [])
  where
    go [] _ (slice, poss) = [(reverse slice, reverse poss)]
    go ([] : rest) (row, col) (slice, poss) = (reverse slice, reverse poss) : go rest (row + 1, 0) ("", [])
    go ((c : cs) : rest) (row, col) (slice, poss)
      | isDigit c = go (cs : rest) (row, col + 1) (c : slice, (row, col) : poss)
      | otherwise = (reverse slice, reverse poss) : go (cs : rest) (row, col + 1) ("", [])

partNumbers :: Schematic -> [Int]
partNumbers schema = do
  let syms = symbols schema
  (num, poss) <- numbers schema
  let neighbs = poss >>= neighbors
  guard $ any (`M.member` syms) neighbs
  return $ read num

gearPartNums :: Schematic -> M.Map (Int, Int) [Int]
gearPartNums schema = M.fromListWith (<>) $ do
  let syms = symbols schema
  let gears = M.filter (== '*') syms
  (num, poss) <- numbers schema
  neighb <- nub (poss >>= neighbors)
  if M.member neighb gears
    then return (neighb, [read num])
    else mempty

part1 :: Schematic -> Int
part1 = sum . partNumbers

part2 :: Schematic -> Int
part2 = sum . M.map product . M.filter ((== 2) . length) . gearPartNums

main :: IO ()
main = do
  inp <- pInp <$> getInput 3
  -- example <- getExample 3 >>= parseIO pInp "day3-example.input"
  putAnswer 3 Part1 (part1 inp)
  putAnswer 3 Part2 (part2 inp)
