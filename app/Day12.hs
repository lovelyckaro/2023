module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 12 >>= parseIO pInp "day12.input"
  -- example <- getExample 12 >>= parseIO pInp "day12-example.input"
  putAnswer 12 Part1 (part1 inp)
  putAnswer 12 Part2 (part2 inp)
