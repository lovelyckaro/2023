module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 6 >>= parseIO pInp "day6.input"
  -- example <- getExample 6 >>= parseIO pInp "day6-example.input"
  putAnswer 6 Part1 (part1 inp)
  putAnswer 6 Part2 (part2 inp)
