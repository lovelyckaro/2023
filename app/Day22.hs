module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 22 >>= parseIO pInp "day22.input"
  -- example <- getExample 22 >>= parseIO pInp "day22-example.input"
  putAnswer 22 Part1 (part1 inp)
  putAnswer 22 Part2 (part2 inp)
