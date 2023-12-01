module Main where

import Data.Char
import Data.Map qualified as M
import Replace.Megaparsec
import SantaLib
import SantaLib.Parsing

pDigit :: Parser Integer
pDigit = do
  d <- digitChar
  return (read [d])

digits :: [(String, Integer)]
digits = zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [1 ..]

pSpelledOutDigit :: Parser Integer
pSpelledOutDigit = choice $ map (\(spelling, digit) -> digit <$ try (string spelling)) digits

pSpelledOutBackwardsDigit :: Parser Integer
pSpelledOutBackwardsDigit = choice $ map (\(spelling, digit) -> digit <$ try (string (reverse spelling))) digits

firstlastdigit :: String -> Integer
firstlastdigit str = read [head digits, last digits]
  where
    digits = filter isDigit str

part1 :: String -> Integer
part1 = sum . map firstlastdigit . lines

firstlastdigit2 :: String -> Integer
firstlastdigit2 line = firstdigit * 10 + lastdigit
  where
    pFirst = pDigit <|> pSpelledOutDigit
    pLast = pDigit <|> pSpelledOutBackwardsDigit
    Just (_prefix, firstdigit, rest) = breakCap pFirst line
    (_suffix, lastdigit, _middle) = case breakCap pLast (reverse rest) of
      Nothing -> (undefined, firstdigit, undefined)
      Just x -> x

part2 = sum . map firstlastdigit2 . lines

main :: IO ()
main = do
  inp <- getInput 1
  putAnswer 1 Part1 (part1 inp)
  putAnswer 1 Part2 (part2 inp)
