module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 8 >>= parseIO pInp "day8.input"
  -- example <- getExample 8 >>= parseIO pInp "day8-example.input"
  putAnswer 8 Part1 (part1 inp)
  putAnswer 8 Part2 (part2 inp)
