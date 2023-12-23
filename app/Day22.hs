{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Monad.State
import Data.List (sortOn)
import Data.Map (Map, (!), (!?))
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import SantaLib
import SantaLib.Parsing hiding (State)

data Range = Range {minX, minY, minZ, maxX, maxY, maxZ :: Int}
  deriving (Show, Eq, Ord)

overlapsWith :: Range -> Range -> Bool
r1 `overlapsWith` r2 =
  not
    ( minX r1 > maxX r2
        || maxX r1 < minX r2
        || minY r1 > maxY r2
        || maxY r1 < minY r2
        || minZ r1 > maxZ r2
        || maxZ r1 < minZ r2
    )

pInp :: Parser [Range]
pInp = some $ lexemeLn $ do
  [minX, minY, minZ] <- lexeme decimal `sepBy` symbol ","
  symbol "~"
  [maxX, maxY, maxZ] <- lexeme decimal `sepBy` symbol ","
  return $ Range {..}

sink :: Range -> Range
sink r = r {minZ = r.minZ - 1, maxZ = r.maxZ - 1}

horizontalPointsIn :: Range -> [(Int, Int)]
horizontalPointsIn r = [(x, y) | x <- [r.minX .. r.maxX], y <- [r.minY .. r.maxY]]

-- | Expose the bricks to gravity until stable
settle :: [Range] -> [Range]
settle = go M.empty [] . sortOn minZ
  where
    overlapsAny :: Range -> [Range] -> Bool
    range `overlapsAny` ranges = any (overlapsWith range) ranges

    go :: Map (Int, Int) Int -> [Range] -> [Range] -> [Range]
    go highest acc [] = acc
    go highest acc (r : rs) =
      let ps = horizontalPointsIn r
          minZ' = maximum $ map (\p -> M.findWithDefault (-1) p highest) ps
          r' = r {minZ = minZ' + 1, maxZ = r.maxZ - (r.minZ - minZ' - 1)}
          highest' = foldr (uncurry (M.insertWith max) . (,r'.maxZ)) highest ps
       in go highest' (r' : acc) rs

supports :: [Range] -> Map Range (Set Range)
supports stack = M.fromList $ map (\r -> (r, S.fromList $ r `supportsHelper` stack)) stack
  where
    supportsHelper :: Range -> [Range] -> [Range]
    supportsHelper r = filter (overlapsWith r {minZ = r.maxZ + 1, maxZ = r.maxZ + 1})

supportedBy :: [Range] -> Map Range (Set Range)
supportedBy stack = M.fromList $ map (\r -> (r, S.fromList $ r `supportedByHelper` stack)) stack
  where
    supportedByHelper :: Range -> [Range] -> [Range]
    supportedByHelper r = filter (overlapsWith r {minZ = r.minZ - 1, maxZ = r.minZ - 1})

canBeDisintegrated :: Map Range (Set Range) -> Map Range (Set Range) -> Range -> Bool
canBeDisintegrated supportsMap supportedByMap r =
  let above = supportsMap ! r
      belowAbove = S.map (supportedByMap !) above
   in all ((> 1) . S.size) belowAbove

part1 :: [Range] -> Int
part1 bricks = length $ filter (canBeDisintegrated supportsMap supportedByMap) stack
  where
    stack = settle bricks
    supportsMap = supports stack
    supportedByMap = supportedBy stack

numAffectedBricks :: Map Range (Set Range) -> Map Range (Set Range) -> [Range] -> Map Range Int
numAffectedBricks supportsMap supportedByMap stack = M.fromList (map (\r -> (r, go (S.singleton r))) stack)
  where
    go :: Set Range -> Int
    go falling =
      let above = S.unions (S.map (supportsMap !) falling) S.\\ falling
          belowAbove = S.map (\r -> (r, supportedByMap ! r S.\\ falling)) above
          willFall = S.map fst $ S.filter (S.null . snd) belowAbove
       in if S.null willFall
            then S.size falling - 1
            else go (falling `S.union` willFall)

part2 :: [Range] -> Int
part2 bricks = sum affectedMap
  where
    stack = settle bricks
    supportsMap = supports stack
    supportedByMap = supportedBy stack
    affectedMap = numAffectedBricks supportsMap supportedByMap stack

main :: IO ()
main = do
  inp <- getInput 22 >>= parseIO pInp "day22.input"
  putAnswer 22 Part1 (part1 inp)
  putAnswer 22 Part2 (part2 inp)
