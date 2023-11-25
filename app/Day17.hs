module Main where

import SantaLib
import SantaLib.Parsing

pInp :: Parser ()
pInp = return ()

part1 = id

part2 = id

main :: IO ()
main = do
  inp <- getInput 17 >>= parseIO pInp "day17.input"
  -- example <- getExample 17 >>= parseIO pInp "day17-example.input"
  putAnswer 17 Part1 (part1 inp)
  putAnswer 17 Part2 (part2 inp)
