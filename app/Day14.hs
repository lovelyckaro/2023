module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 14 >>= parseIO pInp "day14.input"
  -- example <- getExample 14 >>= parseIO pInp "day14-example.input"
  putAnswer 14 Part1 (part1 inp)
  putAnswer 14 Part2 (part2 inp)
