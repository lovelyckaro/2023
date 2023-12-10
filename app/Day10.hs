{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad
import Data.Either
import Data.List
import Data.Map (Map, (!), (!?))
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.Set qualified as S
import SantaLib
import SantaLib.Parsing
import Text.Megaparsec.Char.Lexer qualified as Lex

type Node = (Int, Int)

type Graph = Map Node (Char, Node, Node)

pos :: Parser (Int, Int)
pos = do
  p <- getSourcePos
  return (unPos p.sourceColumn, unPos p.sourceLine)

gSpace :: Parser ()
gSpace = void $ many (string "." <|> eol)

pStart :: Parser (Int, Int)
pStart = Lex.lexeme gSpace (pos <* char 'S')

pToken :: Parser ((Int, Int), (Char, (Int, Int), (Int, Int)))
pToken =
  Lex.lexeme gSpace $
    choice
      [ mkTokenParser c neighbs
        | (c, neighbs) <-
            [ ('|', \(col, line) -> ('|', (col, line - 1), (col, line + 1))),
              ('-', \(col, line) -> ('-', (col - 1, line), (col + 1, line))),
              ('L', \(col, line) -> ('L', (col, line - 1), (col + 1, line))),
              ('J', \(col, line) -> ('J', (col, line - 1), (col - 1, line))),
              ('7', \(col, line) -> ('7', (col, line + 1), (col - 1, line))),
              ('F', \(col, line) -> ('F', (col, line + 1), (col + 1, line)))
            ]
      ]
  where
    mkTokenParser c neighbs = do
      p <- pos
      char c
      return (p, neighbs p)

pInp :: Parser (Node, Graph)
pInp = do
  optional gSpace
  tokens <- some (eitherP pStart pToken)
  let [start] = lefts tokens
  let graphEntries = rights tokens
  eof
  return (start, M.fromList graphEntries)

identifyStart :: Node -> Graph -> Graph
identifyStart s@(col, line) graph =
  let possibleNeighbs = [(col - 1, line), (col + 1, line), (col, line - 1), (col, line + 1)]
      neighbs = zip possibleNeighbs $ map (graph !?) possibleNeighbs
      [neighb1, neighb2] = map fst $ filter (\(neighb, neighbDests) -> maybe False (\(_, x, y) -> x == s || y == s) neighbDests) neighbs
   in M.insert s ('S', neighb1, neighb2) graph

-- order: up, right, down, left
-- graphs first edge goes backwards, second edge forwards

walk :: Node -> Graph -> [Node]
walk start graph = connected (lookupList graph) start
  where
    lookupList :: Graph -> Node -> [Node]
    lookupList g n =
      let (_, node1, node2) = g ! n
       in [node1, node2]

part1 :: (Node, Graph) -> Int
part1 (start, graph) = length (walk start (identifyStart start graph)) `div` 2

part2 :: (Node, Graph) -> Int
part2 (start, graph) = length $ concatMap (filter inside . scanLine verticals) lines
  where
    loop = S.fromList $ walk start (identifyStart start graph)
    verticals = S.filter (\node -> maybe True (\(c, _, _) -> c `elem` "|JL") (graph !? node)) loop
    minLine = 1
    minCol = 1
    maxLine = 140
    maxCol = 140
    lines = [[(c, l) | c <- [minCol .. maxCol]] | l <- [minLine .. maxLine]]
    scanLine walls line = tail $ scanl (\(prevCount, _) node -> if node `S.member` walls then (prevCount + 1, node) else (prevCount, node)) (0, (-1, -1)) line
    inside (vCount, n1) = odd vCount && not (n1 `S.member` loop)

{-
Vi har en lista på noder som är del av loopen.

hörnnoder är meningslösa.

kolla från vänster av grafen till höger
räkna hur många pipes man sett
0 pipes utanför, 1 pipe innanför, 2 pipes utanför, 3 pipes innanför, osv.

Borde räcka med bara ett håll?
-}

main :: IO ()
main = do
  inp <- getInput 10 >>= parseIO pInp "day10.input"
  -- example <- getExample 10 >>= parseIO pInp "day10-example.input"
  putAnswer 10 Part1 (part1 inp)
  putAnswer 10 Part2 (part2 inp)
