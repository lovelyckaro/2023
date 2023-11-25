module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 2 >>= parseIO pInp "day2.input"
  -- example <- getExample 2 >>= parseIO pInp "day2-example.input"
  putAnswer 2 Part1 (part1 inp)
  putAnswer 2 Part2 (part2 inp)
