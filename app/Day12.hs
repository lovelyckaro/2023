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

type SpringInfo = ([SpringToken], [Int])

pRow :: Parser SpringInfo
pRow = do
  tokens <- some (Unknown <$ char '?' <|> Operational <$ char '#' <|> Broken <$ char '.')
  space
  groupSizes <- decimal `sepBy` char ','
  return (tokens, groupSizes)

pInp :: Parser [([SpringToken], [Int])]
pInp = some (lexemeLn pRow) <* eof

numSats2 :: SpringInfo -> Maybe Int -> State (Map (SpringInfo, Maybe Int) Int) Int
numSats2 inp curr = do
  gets (M.lookup (inp, curr)) >>= \case
    Just i -> return i
    Nothing -> do
      val <- case (inp, curr) of
        (([], []), Nothing) -> return 1
        (([], []), Just 0) -> return 1
        (([], sizes), _) -> return 0
        ((Broken : rest, sizes), Just 0) -> numSats2 (rest, sizes) Nothing
        ((Broken : rest, sizes), Just _) -> return 0
        ((Broken : rest, sizes), Nothing) -> numSats2 (rest, sizes) Nothing
        ((Operational : rest, []), Nothing) -> return 0
        ((Operational : rest, sizes), Just 0) -> return 0
        ((Operational : rest, sizes), Just size) -> numSats2 (rest, sizes) (Just (size - 1))
        ((Operational : rest, size : sizes), Nothing) -> numSats2 (rest, sizes) (Just (size - 1))
        ((Unknown : rest, sizes), curr) -> (+) <$> numSats2 (Broken : rest, sizes) curr <*> numSats2 (Operational : rest, sizes) curr
      modify (M.insert (inp, curr) val)
      return val

unfold :: ([SpringToken], [Int]) -> ([SpringToken], [Int])
unfold (tokens, sizes) = (foldl1 (\a b -> a <> (Unknown : b)) (replicate 5 tokens), concat $ replicate 5 sizes)

part1 :: [([SpringToken], [Int])] -> Int
part1 = sum . (`evalState` M.empty) . mapM (`numSats2` Nothing)

part2 :: [([SpringToken], [Int])] -> Int
part2 = part1 . map unfold

main :: IO ()
main = do
  inp <- getInput 12 >>= parseIO pInp "day12.input"
  -- example <- getExample 12 >>= parseIO pInp "day12-example.input"
  putAnswer 12 Part1 (part1 inp)
  putAnswer 12 Part2 (part2 inp)
