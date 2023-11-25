module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 4 >>= parseIO pInp "day4.input"
  -- example <- getExample 4 >>= parseIO pInp "day4-example.input"
  putAnswer 4 Part1 (part1 inp)
  putAnswer 4 Part2 (part2 inp)
