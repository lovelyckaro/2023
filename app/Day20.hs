module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 20 >>= parseIO pInp "day20.input"
  -- example <- getExample 20 >>= parseIO pInp "day20-example.input"
  putAnswer 20 Part1 (part1 inp)
  putAnswer 20 Part2 (part2 inp)
