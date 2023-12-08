module Main where

import Data.List
import Data.Map (Map, (!))
import Data.Map qualified as M
import SantaLib
import SantaLib.Parsing

type Node = String

type Graph = Map Node (Node, Node)

data Direction = L | R
  deriving (Show)

pInp :: Parser ([Direction], Graph)
pInp = do
  directions <- some (L <$ char 'L' <|> R <$ char 'R')
  eol >> eol
  graphElems <- some $ lexemeLn $ do
    node <- lexeme (some upperChar)
    symbol "="
    symbol "("
    left <- lexeme (some upperChar)
    symbol ","
    right <- lexeme (some upperChar)
    symbol ")"
    return (node, (left, right))
  return (directions, M.fromList graphElems)

followDirections :: Graph -> (Node -> Bool) -> [Direction] -> Node -> [Node]
followDirections graph goal (dir : dirs) curr
  | goal curr = []
  | otherwise =
      let (left, right) = graph ! curr
       in case dir of
            L -> left : followDirections graph goal dirs left
            R -> right : followDirections graph goal dirs right

part1 :: ([Direction], Graph) -> Int
part1 (instrs, graph) = length $ followDirections graph (== "ZZZ") (cycle instrs) "AAA"

part2 :: ([Direction], Graph) -> Int
part2 (instrs, graph) = foldr lcm 1 times
  where
    starts = [node | node <- M.keys graph, "A" `isSuffixOf` node]
    times = map (length . followDirections graph ("Z" `isSuffixOf`) (cycle instrs)) starts

main :: IO ()
main = do
  inp <- getInput 8 >>= parseIO pInp "day8.input"
  -- example <- getExample 8 >>= parseIO pInp "day8-example.input"
  putAnswer 8 Part1 (part1 inp)
  putAnswer 8 Part2 (part2 inp)
