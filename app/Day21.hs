module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 21 >>= parseIO pInp "day21.input"
  -- example <- getExample 21 >>= parseIO pInp "day21-example.input"
  putAnswer 21 Part1 (part1 inp)
  putAnswer 21 Part2 (part2 inp)
