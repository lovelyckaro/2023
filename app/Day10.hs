module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 10 >>= parseIO pInp "day10.input"
  -- example <- getExample 10 >>= parseIO pInp "day10-example.input"
  putAnswer 10 Part1 (part1 inp)
  putAnswer 10 Part2 (part2 inp)
