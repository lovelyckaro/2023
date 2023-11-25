module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 13 >>= parseIO pInp "day13.input"
  -- example <- getExample 13 >>= parseIO pInp "day13-example.input"
  putAnswer 13 Part1 (part1 inp)
  putAnswer 13 Part2 (part2 inp)
