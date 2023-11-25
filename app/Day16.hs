module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 16 >>= parseIO pInp "day16.input"
  -- example <- getExample 16 >>= parseIO pInp "day16-example.input"
  putAnswer 16 Part1 (part1 inp)
  putAnswer 16 Part2 (part2 inp)
