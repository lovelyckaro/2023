{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Bifunctor (Bifunctor (first))
import Data.Function
import Data.List
import Data.Ord
import SantaLib
import SantaLib.Parsing

data Card
  = Joker
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Show, Eq, Ord, Enum)

pCard :: Parser Card
pCard = choice [card <$ char c | (c, card) <- zip "23456789TJQKA" [Two .. Ace]]

data HandType
  = HighCard Card [Card]
  | OnePair Card [Card]
  | TwoPair Card Card Card
  | ThreeOfAKind Card [Card]
  | FullHouse Card Card
  | FourOfAKind Card Card
  | FiveOfAKind Card
  deriving (Show, Eq, Ord)

handType :: [Card] -> HandType
handType cards
  | groups == 1 = FiveOfAKind (head cards)
  | groups == 2 = case maximum (map length set) of
      3 ->
        let [ones, others] = set
            one = head ones
            other = head others
         in FullHouse (max one other) (min one other)
      4 ->
        let value = head $ head [c | c <- set, length c == 4]
            other = head $ head [c | c <- set, length c == 1]
         in FourOfAKind value other
  | groups == 3 = case maximum (map length set) of
      3 ->
        let triple = maximumBy (comparing length) set
            value = head triple
         in ThreeOfAKind value [c | c <- cards, c /= value]
      2 ->
        let pairs = filter ((== 2) . length) set
            [value1, value2] = map head pairs
         in TwoPair (max value1 value2) (min value1 value2) (head [c | c <- cards, c /= value1, c /= value2])
  | groups == 4 =
      let pair = maximumBy (comparing length) set
          value = head pair
       in OnePair value [c | c <- cards, c /= value]
  | groups == 5 =
      let highest = maximum cards
       in HighCard highest [c | c <- cards, c /= highest]
  where
    groups = length set
    set = group $ sort cards

-- Assume all jokers will get the same value
replaceJoker :: [Card] -> HandType
replaceJoker hand = maximumBy (comparing devalueHandType) $ do
  replacement <- [Two .. Ace]
  return $ handType (map (\c -> if c == Joker then replacement else c) hand)

-- I made a mistake and assumed that hands would be valued as usual
-- this makes all hands of type Pair equal to each other
devalueHandType :: HandType -> HandType
devalueHandType = \case
  HighCard {} -> HighCard Two []
  OnePair {} -> OnePair Two []
  TwoPair {} -> TwoPair Two Two Two
  ThreeOfAKind {} -> ThreeOfAKind Two []
  FullHouse {} -> FullHouse Two Two
  FourOfAKind {} -> FourOfAKind Two Two
  FiveOfAKind {} -> FiveOfAKind Two

compareHands :: [Card] -> [Card] -> Ordering
compareHands h1 h2 = case comparing (devalueHandType . handType) h1 h2 of
  EQ -> compare h1 h2
  x -> x

compareHands2 :: [Card] -> [Card] -> Ordering
compareHands2 h1 h2 = case comparing (devalueHandType . replaceJoker) h1 h2 of
  EQ -> compare h1 h2
  x -> x

pInp1 :: Parser [([Card], Integer)]
pInp1 = some $ lexemeLn $ do
  hand <- lexeme $ some pCard
  bid <- lexeme decimal
  return (hand, bid)

pInp2 :: Parser [([Card], Integer)]
pInp2 = some $ lexemeLn $ do
  hand <- lexeme $ some ((Joker <$ char 'J') <|> pCard)
  bid <- lexeme decimal
  return (hand, bid)

part1 :: [([Card], Integer)] -> Integer
part1 = sum . zipWith (\pos (hand, bid) -> pos * bid) [1 ..] . sortBy (compareHands `on` fst)

part2 :: [([Card], Integer)] -> Integer
part2 = sum . zipWith (\pos (hand, bid) -> pos * bid) [1 ..] . sortBy (compareHands2 `on` fst)

main :: IO ()
main = do
  inp1 <- getInput 7 >>= parseIO pInp1 "day7.input"
  inp2 <- getInput 7 >>= parseIO pInp2 "day7.input"
  -- example <- getExample 7 >>= parseIO pInp "day7-example.input"
  putAnswer 7 Part1 (part1 inp1)
  putAnswer 7 Part2 (part2 inp2)
