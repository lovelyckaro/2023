module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 18 >>= parseIO pInp "day18.input"
  -- example <- getExample 18 >>= parseIO pInp "day18-example.input"
  putAnswer 18 Part1 (part1 inp)
  putAnswer 18 Part2 (part2 inp)
