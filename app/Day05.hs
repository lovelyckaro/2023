{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Either (lefts, rights)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List.Split (chunksOf)
import SantaLib
import SantaLib.Parsing

data Range = Range {from, to :: Int}
  deriving (Show, Eq)

type Step = Int

data ConversionMap = ConversionMap {name :: String, ranges :: [(Range, Step)]}
  deriving (Show)

pInp :: Parser ([Int], [ConversionMap])
pInp = do
  symbol "seeds:"
  seeds <- some (lexeme decimal)
  eol >> eol
  maps <- pMap `sepBy` eol
  return (seeds, maps)

pMap :: Parser ConversionMap
pMap = do
  n <- takeWhileP (Just "conversion map name") (/= ':')
  symbol ":"
  eol
  ranges <- some $ lexemeLn $ do
    dest <- lexeme decimal
    source <- lexeme decimal
    length <- lexeme decimal
    let from = source
    let to = source + length - 1
    let step = dest - source
    return (Range {..}, step)
  return $ ConversionMap n ranges

applyRange :: Range -> (Range, Step) -> [Either Range Range]
applyRange range (otherrange, step) =
  let before = Range range.from (min (otherrange.from - 1) range.to)
      middle = Range (step + max range.from otherrange.from) (step + min range.to otherrange.to)
      after = Range (max range.from (1 + otherrange.to)) range.to
   in filter (either validRange validRange) [Left before, Right middle, Left after]

validRange :: Range -> Bool
validRange r = r.from <= r.to

applyConversionMap :: Range -> ConversionMap -> [Range]
applyConversionMap range mapping = go [range] mapping.ranges
  where
    go xs [] = xs
    go ranges (mapping : mappings) =
      let results = ranges >>= (`applyRange` mapping)
          done = rights results
          unmatched = lefts results
       in done <> go unmatched mappings

applyConversionMaps :: Range -> [ConversionMap] -> [Range]
applyConversionMaps range mappings = go [range] mappings
  where
    go rs [] = rs
    go rs (mapping : mappings) =
      let newranges = rs >>= (`applyConversionMap` mapping)
       in go newranges mappings

part1 :: ([Int], [ConversionMap]) -> Int
part1 (seeds, mappings) = minimum . map from $ do
  seed <- map (\s -> Range s s) seeds
  applyConversionMaps seed mappings

part2 :: ([Int], [ConversionMap]) -> Int
part2 (seeds, mappings) = minimum . map from $ do
  let initRanges = do
        [from, len] <- chunksOf 2 seeds
        return $ Range from (from + len - 1)
  range <- initRanges
  applyConversionMaps range mappings

main :: IO ()
main = do
  inp <- getInput 5 >>= parseIO pInp "day5.input"
  -- example <- getExample 5 >>= parseIO pInp "day5-example.input"
  putAnswer 5 Part1 (part1 inp)
  putAnswer 5 Part2 (part2 inp)
