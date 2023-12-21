{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad
import Data.Containers.ListUtils (nubOrd)
import Data.Either
import Data.Map (Map)
import Data.Ratio
import Data.Set (Set)
import Data.Set qualified as S
import SantaLib
import SantaLib.Parsing
import Text.Megaparsec.Char.Lexer qualified as Lex

pos :: Parser (Int, Int)
pos = do
  p <- getSourcePos
  return (unPos p.sourceColumn - 1, unPos p.sourceLine - 1)

pInp :: Parser (Set (Int, Int), (Int, Int))
pInp = do
  let graphSpace = void $ many (string "." <|> eol)
  graphSpace
  nodes <- some $ Lex.lexeme graphSpace (eitherP (pos <* string "#") (pos <* string "S"))
  eof
  return (S.fromList $ lefts nodes, head $ rights nodes)

inBounds (x, y) = x >= 0 && x <= 130 && y >= 0 && y <= 130

modBounds :: (Int, Int) -> (Int, Int) -> (Int, Int)
modBounds (x, y) (maxX, maxY) = (x `mod` maxX, y `mod` maxY)

neighbors :: Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
neighbors graph (x, y) = S.fromList $ filter (\p -> p `S.notMember` graph && inBounds p) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

neighbors2 :: Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
neighbors2 graph (x, y) = S.fromList $ filter (\p -> (p `modBounds` (131, 131)) `S.notMember` graph) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

step :: Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
step graph currPoints = S.unions (S.map (neighbors graph) currPoints)

step2 :: Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
step2 graph currPoints = S.unions (S.map (neighbors2 graph) currPoints)

part1 graph start = S.size $ iterate (step graph) (S.singleton start) !! 64

{-
Got completely stuck on part 2
Approach stolen from this comment on reddit:
https://www.reddit.com/r/adventofcode/comments/18nevo3/comment/keaiiq7/?utm_source=share&utm_medium=web2x&context=3
-}

-- $setup
-- >>> (graph, start) <- getInput 21 >>= parseIO pInp ""

-- >>> 26501365 `mod` 131
-- 65

-- >>> 26501365 `div` 131
-- 202300

-- >>> pure . S.size . (!! 65) . iterate (step2 graph) . S.singleton $ start
-- 3868

-- >>> pure . S.size . (!! (65 + 131)) . iterate (step2 graph) . S.singleton $ start
-- 34368

-- >>> pure . S.size . (!! (65 + 131 * 2)) . iterate (step2 graph) . S.singleton $ start
-- 95262

-- f(x) = ax^2 + bx + c
-- f(0) = 3868
-- f(1) = 34368
-- f(2) = 95262
-- 3868 + 15303 x + 15197 x^2

a, b, c :: Rational
a = 15197
b = 15303
c = 3868

f x = a * x ^ 2 + b * x + c

-- >>> f (fromIntegral $ 26501365 `div` 131)
-- 621944727930768 % 1

part2 = numerator $ f $ fromIntegral $ 26501365 `div` 131

main :: IO ()
main = do
  inp <- getInput 21 >>= parseIO pInp "day21.input"
  -- example <- getExample 21 >>= parseIO pInp "day21-example.input"
  putAnswer 21 Part1 (uncurry part1 inp)
  putAnswer 21 Part2 part2
