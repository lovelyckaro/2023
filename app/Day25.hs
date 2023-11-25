module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 25 >>= parseIO pInp "day25.input"
  -- example <- getExample 25 >>= parseIO pInp "day25-example.input"
  putAnswer 25 Part1 (part1 inp)
  putAnswer 25 Part2 (part2 inp)
