module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 11 >>= parseIO pInp "day11.input"
  -- example <- getExample 11 >>= parseIO pInp "day11-example.input"
  putAnswer 11 Part1 (part1 inp)
  putAnswer 11 Part2 (part2 inp)
