module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 1 >>= parseIO pInp "day1.input"
  -- example <- getExample 1 >>= parseIO pInp "day1-example.input"
  putAnswer 1 Part1 (part1 inp)
  putAnswer 1 Part2 (part2 inp)
