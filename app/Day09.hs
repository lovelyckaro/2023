module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 9 >>= parseIO pInp "day9.input"
  -- example <- getExample 9 >>= parseIO pInp "day9-example.input"
  putAnswer 9 Part1 (part1 inp)
  putAnswer 9 Part2 (part2 inp)
