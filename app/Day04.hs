module Main where

import Data.Bifunctor
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import SantaLib
import SantaLib.Parsing

pInp :: Parser [(Set Int, Set Int)]
pInp = some $ lexemeLn $ do
  symbol "Card"
  _id <- lexeme decimal
  symbol ":"
  winning <- some (lexeme decimal)
  symbol "|"
  gotten <- some (lexeme decimal)
  return (S.fromList winning, S.fromList gotten)

matches :: [(Set Int, Set Int)] -> [Int]
matches = map (S.size . uncurry S.intersection)

play :: [Int] -> [Int]
play = go . map (,1)
  where
    go :: [(Int, Int)] -> [Int]
    go [] = []
    go ((match, occs) : rest) =
      let (matched, rest') = splitAt match rest
       in occs : go (map (second (+ occs)) matched <> rest')

part1 :: [(Set Int, Set Int)] -> Integer
part1 = sum . map ((2 ^) . pred) . filter (> 0) . matches

part2 :: [(Set Int, Set Int)] -> Int
part2 = sum . play . matches

main :: IO ()
main = do
  inp <- getInput 4 >>= parseIO pInp "day4.input"
  -- example <- getExample 4 >>= parseIO pInp "day4-example.input"
  putAnswer 4 Part1 (part1 inp)
  putAnswer 4 Part2 (part2 inp)
