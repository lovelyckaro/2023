{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import SantaLib
import SantaLib.Parsing

data Direction = U | R | D | L
  deriving (Show)

data Instr = Instr {direction :: Direction, distance :: Int, color :: String}
  deriving (Show)

pInp :: Parser [Instr]
pInp = some $ lexemeLn $ do
  direction <- lexeme $ choice [U <$ char 'U', R <$ char 'R', D <$ char 'D', L <$ char 'L']
  distance <- lexeme decimal
  color <- between (char '(') (char ')') (char '#' >> some hexDigitChar)
  return $ Instr {..}

outlinePoints :: [Instr] -> Int
outlinePoints = sum . map distance

vertices :: [Instr] -> [(Int, Int)]
vertices instrs = reverse $ go instrs (0, 0)
  where
    go [] p = [p]
    go (i : is) (x, y) =
      let next = case i.direction of
            U -> (x, y + i.distance)
            R -> (x + i.distance, y)
            D -> (x, y - i.distance)
            L -> (x - i.distance, y)
       in (x, y) : go is next

area :: [(Int, Int)] -> Int
area vertices = (`div` 2) $ sum $ zipWith determinant vertices (tail vertices)
  where
    determinant (x1, y1) (x2, y2) = x1 * y2 - y1 * x2

size :: [(Int, Int)] -> Int -> Int
size vertices outline = outline + area vertices - (outline `div` 2) + 1

part1 :: [Instr] -> Int
part1 instrs = size (vertices instrs) (outlinePoints instrs)

swapColor :: Instr -> Instr
swapColor i = Instr direction distance ""
  where
    distance = read $ "0x" <> take 5 i.color
    direction = case last i.color of
      '0' -> R
      '1' -> D
      '2' -> L
      '3' -> U

part2 :: [Instr] -> Int
part2 = part1 . map swapColor

main :: IO ()
main = do
  inp <- getInput 18 >>= parseIO pInp "day18.input"
  -- example <- getExample 18 >>= parseIO pInp "day18-example.input"
  putAnswer 18 Part1 (part1 inp)
  putAnswer 18 Part2 (part2 inp)
