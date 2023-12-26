{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as S
import Data.Hashable
import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import GHC.Generics
import Numeric.LinearAlgebra
import SantaLib
import SantaLib.Parsing
import Prelude hiding (maxBound, minBound)

instance Hashable (Matrix Double) where
  hashWithSalt n = hashWithSalt n . toLists
  hash = hash . toLists

data Line = Line {p, diff :: Matrix Double}
  deriving (Show, Eq, Generic, Hashable)

pInp :: Parser [Line]
pInp = some $ lexemeLn $ do
  coords <- lexeme (signed decimal) `sepBy` symbol ","
  symbol "@"
  diffs <- lexeme (signed decimal) `sepBy` symbol ","
  return $ Line {p = col coords, diff = col diffs}

to2d :: Line -> Line
to2d Line {p, diff} = Line (p ?? (Take 2, All)) (diff ?? (Take 2, All))

-- https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line_segment
intersectionTimes :: Line -> Line -> (Double, Double)
intersectionTimes line1 line2 = (line1Steps, line2Steps)
  where
    numerator = (line1.p - line2.p) ||| negate line2.diff
    denominator = negate line1.diff ||| negate line2.diff
    line1Steps = det numerator / det denominator
    line2Steps = det ((line1.p - line2.p) ||| negate line1.diff) / det denominator

linePoint :: Line -> Double -> Matrix Double
linePoint line steps = line.p + scale steps line.diff

collisions :: [Line] -> HashMap Line [Matrix Double]
collisions lines = HM.fromListWith (++) $ do
  (line1, comparelines) <- zip lines (tails (tail lines))
  line2 <- comparelines
  let (time1, time2) = intersectionTimes line1 line2
  let validTime time = time >= 0 && time /= (1 / 0)
  guard (validTime time1)
  guard (validTime time2)
  let collisionPoint = linePoint line1 time1
  [(line1, [collisionPoint])]

-- | This relic took so many hours
-- turns out that "Hailstones' paths crossed in the past" meant something
-- completely different than I thought
actualCollisions :: Map Double [(Line, Line)] -> HashMap Line (Matrix Double)
actualCollisions collisions = go HM.empty collisions
  where
    go :: HashMap Line (Matrix Double) -> Map Double [(Line, Line)] -> HashMap Line (Matrix Double)
    go alreadyCollided collisions
      | M.null collisions = alreadyCollided
      | otherwise =
          let ((time, willCollide), collisions') = M.deleteFindMin collisions
              alreadyCollided' = HM.union alreadyCollided $ HM.fromList $ do
                (baseLine, collidedWith) <- willCollide
                guard $ not (baseLine `HM.member` alreadyCollided)
                guard $ not (collidedWith `HM.member` alreadyCollided)
                let collisionPoint = linePoint baseLine time
                [(baseLine, collisionPoint), (collidedWith, collisionPoint)]
           in go alreadyCollided' collisions'

inBounds :: Double -> Double -> Matrix Double -> Bool
inBounds min max = all (all (\d -> d >= min && d <= max)) . toLists

minBound :: Double
minBound = 200000000000000

maxBound :: Double
maxBound = 400000000000000

part1 :: [Line] -> Int
part1 = sum . HM.map (length . filter (inBounds minBound maxBound)) . collisions . map to2d

part2 = id

main :: IO ()
main = do
  inp <- getInput 24 >>= parseIO pInp "day24.input"
  -- example <- getExample 24 >>= parseIO pInp "day24-example.input"
  putAnswer 24 Part1 (part1 inp)
  putAnswer 24 Part2 (part2 inp)
