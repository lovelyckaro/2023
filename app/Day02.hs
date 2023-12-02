{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Maybe
import SantaLib
import SantaLib.Parsing

data GrabInfo = GrabInfo {blues, reds, greens :: Int}
  deriving (Show)

data Game = Game {gameId :: Int, amounts :: [GrabInfo]}
  deriving (Show)

pInp :: Parser [Game]
pInp = some (lexemeLn pGame) <* eof

pGame :: Parser Game
pGame = do
  symbol "Game"
  id <- lexeme decimal
  symbol ":"
  amounts <- pAmounts `sepBy` symbol ";"
  return $ Game id amounts

pAmounts :: Parser GrabInfo
pAmounts = do
  let pAmount = do
        amount <- lexeme decimal
        color <- pColor
        return (color, amount)
  table <- pAmount `sepBy` symbol ","
  return $ mkInfo table

mkInfo :: [(String, Int)] -> GrabInfo
mkInfo table =
  GrabInfo
    { blues = fromMaybe 0 (lookup "blue" table),
      reds = fromMaybe 0 (lookup "red" table),
      greens = fromMaybe 0 (lookup "green" table)
    }

pColor :: Parser String
pColor = choice [string "red", string "blue", string "green"]

startingBag :: GrabInfo
startingBag = GrabInfo {reds = 12, greens = 13, blues = 14}

contains :: GrabInfo -> GrabInfo -> Bool
contains bag picked =
  bag.reds >= picked.reds
    && bag.blues >= picked.blues
    && bag.greens >= picked.greens

possible :: GrabInfo -> Game -> Bool
possible amount game = all (amount `contains`) game.amounts

minBag :: Game -> GrabInfo
minBag game = foldr maxEach (GrabInfo 0 0 0) game.amounts
  where
    maxEach amount1 amount2 =
      GrabInfo
        { reds = max amount1.reds amount2.reds,
          blues = max amount1.blues amount2.blues,
          greens = max amount1.greens amount2.greens
        }

power :: GrabInfo -> Int
power info = info.reds * info.blues * info.greens

part1 :: [Game] -> Int
part1 = sum . map gameId . filter (possible startingBag)

part2 :: [Game] -> Int
part2 = sum . map (power . minBag)

main :: IO ()
main = do
  inp <- getInput 2 >>= parseIO pInp "day2.input"
  putAnswer 2 Part1 (part1 inp)
  putAnswer 2 Part2 (part2 inp)
