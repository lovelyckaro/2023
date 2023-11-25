module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 23 >>= parseIO pInp "day23.input"
  -- example <- getExample 23 >>= parseIO pInp "day23-example.input"
  putAnswer 23 Part1 (part1 inp)
  putAnswer 23 Part2 (part2 inp)
