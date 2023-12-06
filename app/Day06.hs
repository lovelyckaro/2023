module Main where

import Data.Function (on)
import Data.List (nubBy)
import SantaLib
import SantaLib.Parsing

pInp :: Parser [(Int, Int)]
pInp = do
  symbol "Time:"
  times <- lexemeLn $ some $ lexeme decimal
  symbol "Distance:"
  distances <- lexemeLn $ some $ lexeme decimal
  return $ zip times distances

calcDistance :: Int -> Int -> Int
calcDistance speed time = speed * time

timeSplits :: Int -> [(Int, Int)]
timeSplits n = [(gassing, going) | gassing <- [1 .. n], let going = n - gassing]

distances :: Int -> [Int]
distances time = map (uncurry calcDistance) $ timeSplits time

waysToBeatRace :: (Int, Int) -> Int
waysToBeatRace (time, dist) = length $ filter (> dist) $ distances time

part1 :: [(Int, Int)] -> Int
part1 = product . map waysToBeatRace

part2 :: [(Int, Int)] -> Int
part2 tups = length $ filter (> combDist) $ distances combTime
  where
    combTime = read $ concatMap (show . fst) tups
    combDist = read $ concatMap (show . snd) tups
    combinedRace = (combTime, combDist)

main :: IO ()
main = do
  inp <- getInput 6 >>= parseIO pInp "day6.input"
  -- example <- getExample 6 >>= parseIO pInp "day6-example.input"
  putAnswer 6 Part1 (part1 inp)
  putAnswer 6 Part2 (part2 inp)
