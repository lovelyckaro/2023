module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 15 >>= parseIO pInp "day15.input"
  -- example <- getExample 15 >>= parseIO pInp "day15-example.input"
  putAnswer 15 Part1 (part1 inp)
  putAnswer 15 Part2 (part2 inp)
