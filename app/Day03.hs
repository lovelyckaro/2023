module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 3 >>= parseIO pInp "day3.input"
  -- example <- getExample 3 >>= parseIO pInp "day3-example.input"
  putAnswer 3 Part1 (part1 inp)
  putAnswer 3 Part2 (part2 inp)
