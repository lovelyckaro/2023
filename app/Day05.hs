module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 5 >>= parseIO pInp "day5.input"
  -- example <- getExample 5 >>= parseIO pInp "day5-example.input"
  putAnswer 5 Part1 (part1 inp)
  putAnswer 5 Part2 (part2 inp)
