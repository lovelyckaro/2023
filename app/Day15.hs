{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Either
import Data.Foldable (Foldable (foldl'))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Map.Ordered (OMap, (<>|), (>|))
import Data.Map.Ordered qualified as OM
import Replace.Megaparsec (splitCap)
import SantaLib
import SantaLib.Parsing

type Boxes = IntMap (OMap String Int)

data Instr = Remove {label :: String} | Set {label :: String, focalValue :: Int}
  deriving (Show)

pInp :: Parser [Instr]
pInp = (pInstr `sepBy` char ',') <* eol <* eof
  where
    pInstr :: Parser Instr
    pInstr = do
      label <- some alphaNumChar
      op <- char '-' <|> char '='
      case op of
        '-' -> return $ Remove label
        '=' -> Set label <$> decimal

handleInstr :: Instr -> Boxes -> Boxes
handleInstr (Remove label) = IM.adjust (OM.delete label) (hash label)
handleInstr (Set label value) = IM.insertWith (<>|) (hash label) (OM.singleton (label, value))

run :: [Instr] -> Boxes
run = foldl' (flip handleInstr) IM.empty

power :: Boxes -> Int
power boxes = sum $ do
  (box, lenses) <- IM.assocs boxes
  ((label, lense), slot) <- zip (reverse $ OM.assocs lenses) [1 ..]
  return $ (box + 1) * slot * lense

charValue :: Int -> Char -> Int
charValue prevValue c = ((prevValue + fromEnum c) * 17) `mod` 256

hash :: String -> Int
hash = foldl' charValue 0

part1 :: String -> Int
part1 = sum . map hash . lefts . splitCap (char ',' :: Parser Char) . filter (/= '\n')

part2 :: [Instr] -> Int
part2 = power . run

main :: IO ()
main = do
  inp <- getInput 15
  -- example <- getExample 15 >>= parseIO pInp "day15-example.input"
  instrs <- parseIO pInp "day15.input" inp
  putAnswer 15 Part1 (part1 inp)
  putAnswer 15 Part2 (part2 instrs)
