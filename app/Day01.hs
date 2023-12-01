module Main where

import Control.Monad
import Data.List
import Data.Map (Map, (!))
import Data.Map qualified as M
import SantaLib

pDigits :: Map String Integer -> String -> [Integer]
pDigits digitTable line = [toNum digit | slice <- tails line, digit <- digits, digit `isPrefixOf` slice]
  where
    toNum n = digitTable ! n
    digits = M.keys digitTable

digits1 :: Map String Integer
digits1 = M.fromList [(show n, n) | n <- [1 .. 9]]

digits2 :: Map String Integer
digits2 = digits1 `M.union` M.fromList (zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [1 .. 9])

firstlastdigit :: Map String Integer -> String -> Integer
firstlastdigit digitTable str = head digits * 10 + last digits
  where
    digits = pDigits digitTable str

part1 :: String -> Integer
part1 = sum . map (firstlastdigit digits1) . lines

part2 :: String -> Integer
part2 = sum . map (firstlastdigit digits2) . lines

main :: IO ()
main = do
  inp <- getInput 1
  putAnswer 1 Part1 (part1 inp)
  putAnswer 1 Part2 (part2 inp)
