{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad
import Data.Either
import Data.Foldable (find)
import Data.List
import Replace.Megaparsec (sepCap, splitCap)
import SantaLib
import SantaLib.Parsing

pInp :: String -> [[String]]
pInp inp = do
  graph <- lefts $ splitCap (eol >> eol :: Parser String) inp
  return $ lines graph

isReflectionRow :: [String] -> Int -> Bool
isReflectionRow graph col =
  let (before, after) = splitAt col graph
   in and (zipWith (==) (reverse before) after)

isReflectionRow2 :: [String] -> Int -> Bool
isReflectionRow2 graph col =
  let (before, after) = splitAt col graph
   in 1 == countFalse (zipWith (==) (concat $ reverse before) (concat after))

reflectionRow :: [String] -> Maybe Int
reflectionRow graph = find (isReflectionRow graph) [1 .. length graph - 1]

reflectionCol :: [String] -> Maybe Int
reflectionCol = reflectionRow . transpose

reflectionRow2 :: [String] -> Maybe Int
reflectionRow2 graph = find (isReflectionRow2 graph) [1 .. length graph - 1]

reflectionCol2 :: [String] -> Maybe Int
reflectionCol2 = reflectionRow2 . transpose

countFalse :: [Bool] -> Int
countFalse = length . filter not

score :: [String] -> Int
score graph = case reflectionCol graph of
  Just c -> c
  Nothing -> case reflectionRow graph of
    Just r -> 100 * r
    Nothing -> error "No reflection in graph"

score2 :: [String] -> Int
score2 graph = case reflectionCol2 graph of
  Just c -> c
  Nothing -> case reflectionRow2 graph of
    Just r -> 100 * r
    Nothing -> error "No reflection in graph"

part1 :: [[String]] -> Int
part1 = sum . map score

part2 = sum . map score2

main :: IO ()
main = do
  inp <- pInp <$> getInput 13
  -- example <- getExample 13 >>= parseIO pInp "day13-example.input"
  putAnswer 13 Part1 (part1 inp)
  putAnswer 13 Part2 (part2 inp)
