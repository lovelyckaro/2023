module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 24 >>= parseIO pInp "day24.input"
  -- example <- getExample 24 >>= parseIO pInp "day24-example.input"
  putAnswer 24 Part1 (part1 inp)
  putAnswer 24 Part2 (part2 inp)
