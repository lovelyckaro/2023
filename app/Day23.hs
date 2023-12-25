{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad
import Data.Map (Map, (!), (!?))
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import SantaLib
import SantaLib.Parsing
import Text.Megaparsec.Char.Lexer qualified as Lex

data Direction = Undirected | North | East | South | West
  deriving (Show, Eq, Ord)

type Point = (Int, Int)

pos :: Parser Point
pos = do
  p <- getSourcePos
  return (unPos p.sourceColumn, unPos p.sourceLine)

pInp :: Parser (Map Point Direction)
pInp = do
  let graphSpace = void $ many (string "#" <|> eol)
  graphSpace
  nodes <- some $ Lex.lexeme graphSpace $ (,) <$> pos <*> choice [Undirected <$ char '.', North <$ char '^', East <$ char '>', South <$ char 'v', West <$ char '<']
  eof
  return $ M.fromList nodes

neighbors :: Map Point Direction -> Point -> [Point]
neighbors graph (x, y) = filter (`M.member` graph) $ case graph !? (x, y) of
  Nothing -> []
  Just Undirected -> [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  Just North -> [(x, y - 1)]
  Just East -> [(x + 1, y)]
  Just South -> [(x, y + 1)]
  Just West -> [(x - 1, y)]

allNeighbors :: Map Point Direction -> Point -> [Point]
allNeighbors graph (x, y) = filter (`M.member` graph) $ case graph !? (x, y) of
  Nothing -> []
  Just _ -> [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

longestPath :: Map Point Direction -> Maybe Int
longestPath graph = go S.empty (fst $ M.findMin graph)
  where
    goal = fst $ M.findMax graph
    go :: Set Point -> Point -> Maybe Int
    go visited curr
      | curr == goal = Just 0
      | otherwise = case filter (`S.notMember` visited) $ neighbors graph curr of
          [] -> Nothing
          neighbs -> (1 +) <$> maximum (map (go (S.insert curr visited)) neighbs)

nextJunction :: Map Point Direction -> Point -> Point -> (Int, Point)
nextJunction graph prev curr = case [neighb | neighb <- allNeighbors graph curr, neighb /= prev] of
  [next] ->
    let (len, junction) = nextJunction graph curr next
     in (len + 1, junction)
  _ -> (1, curr)

junctionNeighbors :: Map Point Direction -> Point -> [(Int, Point)]
junctionNeighbors graph curr = [nextJunction graph curr neighb | neighb <- allNeighbors graph curr]

junctionGraph :: Map Point Direction -> Map Point [(Int, Point)]
junctionGraph graph = go M.empty [fst $ M.findMin graph]
  where
    go visited [] = visited
    go visited (curr : stack) =
      let neighbs = junctionNeighbors graph curr
          unvisited = [neighb | (_, neighb) <- neighbs, neighb `M.notMember` visited]
       in go (M.insert curr neighbs visited) (stack <> unvisited)

longestPath2 :: Map Point [(Int, Point)] -> Maybe Int
longestPath2 graph = go S.empty (fst $ M.findMin graph)
  where
    goal = fst $ M.findMax graph
    go :: Set Point -> Point -> Maybe Int
    go visited curr
      | curr == goal = Just 0
      | otherwise = case [(dist, neighb) | (dist, neighb) <- graph ! curr, neighb `S.notMember` visited] of
          [] -> Nothing
          neighbs -> maximum [(dist +) <$> go (S.insert curr visited) neighb | (dist, neighb) <- neighbs]

junctions :: Map Point Direction -> [Point]
junctions graph = connected (map snd . junctionNeighbors graph) (fst $ M.findMin graph)

part1 :: Map Point Direction -> Int
part1 = fromJust . longestPath

part2 :: Map Point Direction -> Int
part2 = fromJust . longestPath2 . junctionGraph

main :: IO ()
main = do
  inp <- getInput 23 >>= parseIO pInp "day23.input"
  -- example <- getExample 23 >>= parseIO pInp "day23-example.input"
  putAnswer 23 Part1 (part1 inp)
  putAnswer 23 Part2 (part2 inp)
