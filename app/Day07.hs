module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 7 >>= parseIO pInp "day7.input"
  -- example <- getExample 7 >>= parseIO pInp "day7-example.input"
  putAnswer 7 Part1 (part1 inp)
  putAnswer 7 Part2 (part2 inp)
