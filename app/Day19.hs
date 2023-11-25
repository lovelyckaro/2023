module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 19 >>= parseIO pInp "day19.input"
  -- example <- getExample 19 >>= parseIO pInp "day19-example.input"
  putAnswer 19 Part1 (part1 inp)
  putAnswer 19 Part2 (part2 inp)
