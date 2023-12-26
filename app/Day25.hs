module Main where

import Control.Monad.State
import Data.Containers.ListUtils
import Data.Graph.Inductive
import Data.HashSet (HashSet)
import Data.HashSet qualified as S
import SantaLib
import SantaLib.Parsing hiding (State, empty, match)
import System.Random.Stateful

pInp :: Parser [(String, [String])]
pInp = some $ lexemeLn $ do
  node <- lexeme $ some lowerChar
  symbol ":"
  neighbs <- some lowerChar `sepBy` space
  return (node, neighbs)

toGraph :: [(String, [String])] -> Gr (HashSet String) ()
toGraph nodes = run_ empty $ do
  insMapNodesM $ nubOrd [S.singleton node | (n, neighbs) <- nodes, node <- n : neighbs]
  insMapEdgesM $ [(S.singleton node, S.singleton neighb, ()) | (node, neighbs) <- nodes, neighb <- neighbs]

pickEdge :: (StatefulGen g m) => Gr (HashSet String) () -> g -> m Edge
pickEdge graph randomGen = do
  let es = edges graph
  edge <- uniformRM (0, length es - 1) randomGen
  return $ es !! edge

mergeEdge :: Edge -> Gr (HashSet String) () -> Gr (HashSet String) ()
mergeEdge (from, to) graph =
  let (Just fromContext, rest) = match from graph
      (Just toContext, rest') = match to rest
      adj =
        [ ((), node)
          | (node, ()) <- lpre' fromContext <> lsuc' fromContext,
            node /= to
        ]
          <> [((), node) | (node, ()) <- lpre' toContext <> lsuc' toContext]
      newContext = ([], from, lab' fromContext `S.union` lab' toContext, adj)
   in newContext & rest'

contract :: (StatefulGen g m) => Gr (HashSet String) () -> g -> m (Gr (HashSet String) ())
contract graph g
  | noNodes graph == 2 = return graph
  | otherwise = do
      edge <- pickEdge graph g
      contract (mergeEdge edge graph) g

part1 :: [(String, [String])] -> Int
part1 nodes = product [S.size nodes | (_, nodes) <- labNodes actualSolution]
  where
    actualSolution = runStateGen_ (mkStdGen 420) (go possibleSolution)
    possibleSolution = contract $ toGraph nodes
    go genSolution gen = do
      solution <- genSolution gen
      if length (edges solution) == 3
        then return solution
        else go possibleSolution gen

main :: IO ()
main = do
  inp <- getInput 25 >>= parseIO pInp "day25.input"
  -- example <- getExample 25 >>= parseIO pInp "day25-example.input"
  putAnswer 25 Part1 (part1 inp)
