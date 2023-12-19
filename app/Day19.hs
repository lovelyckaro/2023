{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Foldable (find)
import Data.Map (Map, (!), (!?))
import Data.Map qualified as M
import Data.Maybe
import SantaLib hiding (Part)
import SantaLib.Parsing
import Prelude hiding (GT, LT)

data PartProperty = X | M | A | S
  deriving (Show, Eq, Ord, Enum)

type Part = Map PartProperty Int

data Label where
  Accept :: Label
  Reject :: Label
  RuleName :: String -> Label
  deriving (Show, Eq, Ord)

data Rule where
  LT :: PartProperty -> Int -> Label -> Rule
  GT :: PartProperty -> Int -> Label -> Rule
  Always :: Label -> Rule
  deriving (Show)

data RuleStatement = RuleStatement {name :: String, rules :: [Rule]}
  deriving (Show)

pProperty :: Parser PartProperty
pProperty = choice [X <$ symbol "x", M <$ symbol "m", A <$ symbol "a", S <$ symbol "s"]

pLabel :: Parser Label
pLabel = choice [Accept <$ symbol "A", Reject <$ symbol "R", RuleName <$> some lowerChar]

pRule :: Parser Rule
pRule =
  choice
    [ try $ LT <$> pProperty <* symbol "<" <*> decimal <* symbol ":" <*> pLabel,
      try $ GT <$> pProperty <* symbol ">" <*> decimal <* symbol ":" <*> pLabel,
      Always <$> pLabel
    ]

pRuleStatement :: Parser RuleStatement
pRuleStatement = do
  name <- some lowerChar
  rules <- between (symbol "{") (symbol "}") (pRule `sepBy` symbol ",")
  return $ RuleStatement {..}

pPart :: Parser Part
pPart =
  M.fromList
    <$> between
      (symbol "{")
      (symbol "}")
      (((,) <$> pProperty <* symbol "=" <*> decimal) `sepBy` symbol ",")

pInp :: Parser (Map String RuleStatement, [Part])
pInp = do
  ruleStatements <- some $ lexemeLn pRuleStatement
  let ruleMap = M.fromList $ zip (map name ruleStatements) ruleStatements
  eol
  parts <- some $ lexemeLn pPart
  eof
  return (ruleMap, parts)

startingRule :: Label
startingRule = RuleName "in"

step :: Map String RuleStatement -> Part -> Label -> Label
step rules part Accept = Accept
step rules part Reject = Reject
step rules part (RuleName rule) =
  let RuleStatement _name conditions = rules ! rule
   in fromJust $ foldr ((<|>) . apply part) Nothing conditions

run :: Map String RuleStatement -> Part -> Label
run rules part = fromJust $ find done $ iterate (step rules part) startingRule
  where
    done (RuleName _) = False
    done Accept = True
    done Reject = True

apply :: Part -> Rule -> Maybe Label
apply p (Always label) = Just label
apply p (LT property value label)
  | p ! property < value = Just label
  | otherwise = Nothing
apply p (GT property value label)
  | p ! property > value = Just label
  | otherwise = Nothing

data Range a = Range {from, to :: a}
  deriving (Show)

type RangePart = Map PartProperty (Range Int)

applyRange :: RangePart -> Rule -> [(RangePart, Maybe Label)]
applyRange p (Always label) = [(p, Just label)]
applyRange p (LT property value label)
  | (p ! property).from >= value = [(p, Nothing)]
  | (p ! property).to < value = [(p, Just label)]
  | otherwise = [(M.insert property (Range (p ! property).from (value - 1)) p, Just label), (M.insert property (Range value (p ! property).to) p, Nothing)]
applyRange p (GT property value label)
  | (p ! property).to <= value = [(p, Nothing)]
  | (p ! property).from > value = [(p, Just label)]
  | otherwise = [(M.insert property (Range (p ! property).from value) p, Nothing), (M.insert property (Range (value + 1) (p ! property).to) p, Just label)]

applyRuleStmt :: [Rule] -> RangePart -> [(RangePart, Label)]
applyRuleStmt (rule : rest) part = do
  (match, mbLabel) <- applyRange part rule
  case mbLabel of
    Just Reject -> []
    Just label -> return (match, label)
    Nothing -> applyRuleStmt rest match

stepRange :: Map String RuleStatement -> RangePart -> Label -> [(RangePart, Label)]
stepRange rules part Accept = [(part, Accept)]
stepRange rules part Reject = []
stepRange rules part (RuleName name) = applyRuleStmt (rules ! name).rules part

runRange :: Map String RuleStatement -> RangePart -> Label -> [RangePart]
runRange rules part label = do
  (match, label') <- stepRange rules part label
  case label of
    Accept -> return match
    Reject -> [] -- shouldn't be possible but whatever
    RuleName _ -> runRange rules match label'

part1 :: Map String RuleStatement -> [Part] -> Int
part1 rules parts = sum $ map sum $ filter ((== Accept) . run rules) parts

startingPart :: RangePart
startingPart = M.fromList $ map (,Range {from = 1, to = 4000}) [X .. S]

distinctParts :: RangePart -> Int
distinctParts = product . M.map (\r -> r.to - r.from + 1)

part2 :: Map String RuleStatement -> Int
part2 rules = sum . map distinctParts $ runRange rules startingPart startingRule

main :: IO ()
main = do
  inp <- getInput 19 >>= parseIO pInp "day19.input"
  -- example <- getExample 19 >>= parseIO pInp "day19-example.input"
  putAnswer 19 Part1 (uncurry part1 inp)
  putAnswer 19 Part2 (part2 (fst inp))
