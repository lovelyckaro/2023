{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.State
import Data.Map (Map, (!), (!?))
import Data.Map qualified as M
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import SantaLib
import SantaLib.Parsing hiding (State)
import Prelude hiding (flip)

data Pulse = Low | High
  deriving (Show, Eq)

data FlipFlopState = On | Off
  deriving (Show)

data Module
  = Broadcaster [String]
  | FlipFlop FlipFlopState [String]
  | Conjunction (Map String Pulse) [String]
  deriving (Show)

pModule :: Parser (String, Module)
pModule = choice [pFlipFlop, pConjunction, pBroadcaster]
  where
    pBroadcaster = do
      name <- symbol "broadcaster"
      symbol "->"
      outputs <- some lowerChar `sepBy` symbol ","
      return (name, Broadcaster outputs)
    pFlipFlop = do
      char '%'
      name <- lexeme (some lowerChar)
      symbol "->"
      outputs <- some lowerChar `sepBy` symbol ","
      return (name, FlipFlop Off outputs)
    pConjunction = do
      char '&'
      name <- lexeme (some lowerChar)
      symbol "->"
      outputs <- some lowerChar `sepBy` symbol ","
      -- conjunction last seen inputs is updated in a later state
      return (name, Conjunction M.empty outputs)

pInp :: Parser (Map String Module)
pInp = M.fromList <$> some (lexemeLn pModule) <* eof

outputs :: Module -> [String]
outputs (Broadcaster outs) = outs
outputs (FlipFlop _ outs) = outs
outputs (Conjunction _ outs) = outs

populateConjunctionInputs :: State SystemState ()
populateConjunctionInputs = do
  mods <- gets (M.assocs . modules)
  forM_ mods $ \(name, mod) ->
    forM_ (outputs mod) $ \dest ->
      gets ((!? dest) . modules) >>= \case
        Just (Conjunction inputs outputs) -> modify (\s -> s {modules = M.insert dest (Conjunction (M.insert name Low inputs) outputs) (modules s)})
        _ -> return ()

data SystemState = SystemState
  { signalQueue :: Seq (String, Pulse, String),
    modules :: Map String Module,
    numLow, numHigh :: Int,
    rxPresses :: Int
  }
  deriving (Show)

initialQueue :: Seq (String, Pulse, String)
initialQueue = Empty

initSystemState :: Map String Module -> SystemState
initSystemState modules =
  SystemState
    { signalQueue = initialQueue,
      modules = modules,
      numLow = 0,
      numHigh = 0,
      rxPresses = 0
    }

flip :: Module -> Module
flip (FlipFlop On outs) = FlipFlop Off outs
flip (FlipFlop Off outs) = FlipFlop On outs

queue :: Seq (String, Pulse, String) -> State SystemState ()
queue seq = modify $ \s -> s {signalQueue = signalQueue s <> seq}

popQueue :: State SystemState (Maybe (String, Pulse, String))
popQueue =
  gets signalQueue >>= \case
    Empty -> return Nothing
    (signal@(_, pulse, _) :<| rest) -> do
      case pulse of
        High -> modify $ \s -> s {numHigh = numHigh s + 1}
        Low -> modify $ \s -> s {numLow = numLow s + 1}
      modify $ \s -> s {signalQueue = rest}
      return $ Just signal

modifyModule :: String -> (Module -> Module) -> State SystemState ()
modifyModule name f =
  gets ((!? name) . modules) >>= \case
    Nothing -> return ()
    Just mod -> modify $ \s -> s {modules = M.insert name (f mod) (modules s)}

getsModule :: String -> (Module -> a) -> State SystemState (Maybe a)
getsModule name f =
  gets ((!? name) . modules) >>= \case
    Nothing -> return Nothing
    Just mod -> return $ Just $ f mod

putModule :: String -> Module -> State SystemState ()
putModule name mod = modify $ \s -> s {modules = M.insert name mod (modules s)}

tick :: State SystemState ()
tick = do
  popQueue >>= \case
    Nothing -> return ()
    Just (from, pulse, to) ->
      gets ((!? to) . modules) >>= \case
        Nothing ->
          when (pulse == Low && to == "rx") $ modify $ \s -> s {rxPresses = rxPresses s + 1}
        Just (Broadcaster outs) ->
          queue $ Seq.fromList $ map (to,pulse,) outs
        Just (FlipFlop state outs) ->
          when (pulse == Low) $ do
            modifyModule to flip
            let p = case state of
                  On -> Low
                  Off -> High
            queue $ Seq.fromList $ map (to,p,) outs
        Just (Conjunction ins outs) -> do
          let ins' = M.insert from pulse ins
          putModule to (Conjunction ins' outs)
          if all (== High) ins'
            then queue $ Seq.fromList $ map (to,Low,) outs
            else queue $ Seq.fromList $ map (to,High,) outs

run :: State SystemState ()
run =
  gets signalQueue >>= \case
    Empty -> return ()
    _ -> tick >> run

pressButton :: State SystemState ()
pressButton = do
  queue (Seq.singleton ("button", Low, "broadcaster"))
  run

untilRxPressed :: State SystemState ()
untilRxPressed = do
  gets rxPresses >>= \case
    0 -> pressButton >> untilRxPressed
    n -> return ()

part1 :: Map String Module -> Int
part1 modules =
  let finalState = execState (populateConjunctionInputs >> replicateM_ 1000 pressButton) $ initSystemState modules
   in numLow finalState * numHigh finalState

part2 = execState (populateConjunctionInputs >> untilRxPressed) . initSystemState

main :: IO ()
main = do
  inp <- getInput 20 >>= parseIO pInp "day20.input"
  -- example <- getExample 20 >>= parseIO pInp "day20-example.input"
  putAnswer 20 Part1 (part1 inp)
  putAnswer 20 Part2 (part2 inp)
