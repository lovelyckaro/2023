{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.State
import Data.Bifunctor
import Data.List (group)
import Data.Map (Map)
import Data.Map qualified as M
import SantaLib
import SantaLib.Parsing hiding (State)

data SpringToken = Broken | Operational | Unknown
  deriving (Show, Eq, Ord)

pRow :: Parser ([SpringToken], [Int])
pRow = do
  tokens <- some (Unknown <$ char '?' <|> Operational <$ char '#' <|> Broken <$ char '.')
  space
  groupSizes <- decimal `sepBy` char ','
  return (tokens, groupSizes)

pInp :: Parser [([SpringToken], [Int])]
pInp = some (lexemeLn pRow) <* eof

numSats :: ([SpringToken], [Either Int Int]) -> Int
numSats ([], []) = 1
numSats ([], [Left 0]) = 1
numSats ([], sizes) = 0
numSats (Broken : rest, Left 0 : sizes) = numSats (rest, sizes)
numSats (Broken : rest, Left n : sizes) = 0
numSats (Broken : rest, sizes) = numSats (rest, sizes)
numSats (Operational : rest, []) = 0
numSats (Operational : rest, Left 0 : sizes) = 0
numSats (Operational : rest, Left size : sizes) = numSats (rest, Left (size - 1) : sizes)
numSats (Operational : rest, Right size : sizes) = numSats (rest, Left (size - 1) : sizes)
numSats (Unknown : rest, sizes) = numSats (Broken : rest, sizes) + numSats (Operational : rest, sizes)

numSats2 :: ([SpringToken], [Either Int Int]) -> State (Map ([SpringToken], [Either Int Int]) Int) Int
numSats2 inp = do
  gets (M.lookup inp) >>= \case
    Just i -> return i
    Nothing -> do
      val <- case inp of
        ([], []) -> return 1
        ([], [Left 0]) -> return 1
        ([], sizes) -> return 0
        (Broken : rest, Left 0 : sizes) -> numSats2 (rest, sizes)
        (Broken : rest, Left n : sizes) -> return 0
        (Broken : rest, sizes) -> numSats2 (rest, sizes)
        (Operational : rest, []) -> return 0
        (Operational : rest, Left 0 : sizes) -> return 0
        (Operational : rest, Left size : sizes) -> numSats2 (rest, Left (size - 1) : sizes)
        (Operational : rest, Right size : sizes) -> numSats2 (rest, Left (size - 1) : sizes)
        (Unknown : rest, sizes) -> (+) <$> numSats2 (Broken : rest, sizes) <*> numSats2 (Operational : rest, sizes)
      modify (M.insert inp val)
      return val

unfold :: ([SpringToken], [Int]) -> ([SpringToken], [Int])
unfold (tokens, sizes) = (foldl1 (\a b -> a <> (Unknown : b)) (replicate 5 tokens), concat $ replicate 5 sizes)

part1 :: [([SpringToken], [Int])] -> Int
part1 = sum . (`evalState` M.empty) . mapM (numSats2 . second (map Right))

part2 :: [([SpringToken], [Int])] -> Int
part2 = part1 . map unfold

main :: IO ()
main = do
  inp <- getInput 12 >>= parseIO pInp "day12.input"
  -- example <- getExample 12 >>= parseIO pInp "day12-example.input"
  putAnswer 12 Part1 (part1 inp)
  putAnswer 12 Part2 (part2 inp)
