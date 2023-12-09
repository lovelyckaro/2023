module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser [[Int]]
pInp = some $ lexemeLn $ some $ lexeme $ signed decimal

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) (tail xs) xs

allDiffs :: [Int] -> [[Int]]
allDiffs xs
  | all (== 0) (diffs xs) = [diffs xs]
  | otherwise =
      let ds = diffs xs
       in ds : allDiffs ds

extrapolateNext :: [Int] -> Int
extrapolateNext xs = sum (map last (xs : allDiffs xs))

subHalf :: [Int] -> Int
subHalf [] = 0
subHalf [x] = x
subHalf (x : y : xs) = x - y + subHalf xs

extrapolatePrev :: [Int] -> Int
extrapolatePrev xs = subHalf (map head (xs : allDiffs xs))

part1 :: [[Int]] -> Int
part1 = sum . map extrapolateNext

part2 = sum . map extrapolatePrev

main :: IO ()
main = do
  inp <- getInput 9 >>= parseIO pInp "day9.input"
  -- example <- getExample 9 >>= parseIO pInp "day9-example.input"
  putAnswer 9 Part1 (part1 inp)
  putAnswer 9 Part2 (part2 inp)
