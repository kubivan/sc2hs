{-# LANGUAGE OverloadedStrings #-}

module SC2.Army.StrategyDetection
  ( EnemyStrategy (..)
  , DiagnosticEvent (..)
  , StrategyBelief (..)
  , emptyBelief
  , updateWithEvent
  , recognizedStrategy
  , allPosteriors
  , diagnosticTrace
  ) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Hashable (Hashable, hashWithSalt)

-- | Enemy strategic archetypes
data EnemyStrategy
  = Macro
  | Cheese
  | TimingRush
  | BuildOrder
  deriving (Eq, Show, Ord)

instance Hashable EnemyStrategy where
  hashWithSalt s Macro = hashWithSalt s (0 :: Int)
  hashWithSalt s Cheese = hashWithSalt s (1 :: Int)
  hashWithSalt s TimingRush = hashWithSalt s (2 :: Int)
  hashWithSalt s BuildOrder = hashWithSalt s (3 :: Int)

-- | High-confidence diagnostic events revealing strategy
data DiagnosticEvent
  = ProxyStructureNearBase
  | EarlyExpansion Int
  | GasBuildEarly Int
  | AggresiveUnitMass Int Int
  | KeyStructureBuilt Int Int
  | EnemyTechBuilding Int
  deriving (Show)

-- | Belief state: posterior probabilities over strategies
data StrategyBelief = StrategyBelief
  { sbPosterior :: HashMap EnemyStrategy Float
  , sbConfidence :: Float
  , sbRecognized :: Maybe EnemyStrategy
  , sbDiagnosticTrace :: [Text]
  , sbGameLoop :: Int
  } deriving (Show)

-- | Initialize with uniform prior
emptyBelief :: StrategyBelief
emptyBelief = StrategyBelief
  { sbPosterior = HashMap.fromList
      [ (Macro, 0.25)
      , (Cheese, 0.25)
      , (TimingRush, 0.25)
      , (BuildOrder, 0.25)
      ]
  , sbConfidence = 0.25
  , sbRecognized = Nothing
  , sbDiagnosticTrace = []
  , sbGameLoop = 0
  }

-- | Update belief with diagnostic event
updateWithEvent :: DiagnosticEvent -> StrategyBelief -> StrategyBelief
updateWithEvent ev belief =
  let strategies = [Macro, Cheese, TimingRush, BuildOrder]
      impacts = [(s, eventBoost ev s) | s <- strategies]
      newPost = applyBoosts impacts (sbPosterior belief)
      maxConf = maximum (HashMap.elems newPost)
      recognized = if maxConf > 0.75
                     then Just (fst $ maximumBy (comparing snd) (HashMap.toList newPost))
                     else Nothing
      trace = sbDiagnosticTrace belief ++ [formatEvent ev maxConf]
  in StrategyBelief
       { sbPosterior = newPost
       , sbConfidence = maxConf
       , sbRecognized = recognized
       , sbDiagnosticTrace = trace
       , sbGameLoop = sbGameLoop belief + 1
       }

-- | Multiplicative boost factor: how much does event support strategy?
eventBoost :: DiagnosticEvent -> EnemyStrategy -> Float
-- Proxy near base is near-certain Cheese indicator
eventBoost ProxyStructureNearBase Cheese = 10.0
eventBoost ProxyStructureNearBase _ = 0.1

-- Early expansion (before ~2400 frames) is Macro
eventBoost (EarlyExpansion f) Macro
  | f < 2400 = 5.0
  | otherwise = 1.0
eventBoost (EarlyExpansion _) _ = 0.8

-- Early gas is economic (Macro) focus
eventBoost (GasBuildEarly _) Macro = 3.0
eventBoost (GasBuildEarly _) _ = 0.9

-- Aggressive unit mass suggests TimingRush
eventBoost (AggresiveUnitMass _ count) TimingRush
  | count > 15 = 4.0
eventBoost (AggresiveUnitMass _ _) _ = 1.0

-- Key structure (robo, bay, etc) suggests BuildOrder path
eventBoost (KeyStructureBuilt _ _) BuildOrder = 2.0
eventBoost (KeyStructureBuilt _ _) _ = 1.0

-- Tech building suggests macro or timing prep
eventBoost (EnemyTechBuilding _) Macro = 2.0
eventBoost (EnemyTechBuilding _) TimingRush = 0.7
eventBoost (EnemyTechBuilding _) _ = 1.0

-- | Apply multiplicative boosts and renormalize posterior
applyBoosts :: [(EnemyStrategy, Float)] -> HashMap EnemyStrategy Float -> HashMap EnemyStrategy Float
applyBoosts impacts post =
  let boosted = HashMap.fromList [(s, HashMap.findWithDefault 0.25 s post * boost) | (s, boost) <- impacts]
      z = sum (HashMap.elems boosted)
  in if z > 0 then HashMap.map (/ z) boosted else post

-- | Format diagnostic event for trace log
formatEvent :: DiagnosticEvent -> Float -> Text
formatEvent ProxyStructureNearBase conf = 
  "ProxyDetected (conf=" <> showFloat conf <> ")"
formatEvent (EarlyExpansion f) conf = 
  "EarlyExpansion @" <> T.pack (show f) <> " (conf=" <> showFloat conf <> ")"
formatEvent (GasBuildEarly f) conf = 
  "GasBuild @" <> T.pack (show f) <> " (conf=" <> showFloat conf <> ")"
formatEvent (AggresiveUnitMass uid cnt) conf = 
  "UnitMass " <> T.pack (show uid) <> "x" <> T.pack (show cnt) <> " (conf=" <> showFloat conf <> ")"
formatEvent (KeyStructureBuilt uid f) conf = 
  "Structure " <> T.pack (show uid) <> " @" <> T.pack (show f) <> " (conf=" <> showFloat conf <> ")"
formatEvent (EnemyTechBuilding f) conf = 
  "TechBuilding @" <> T.pack (show f) <> " (conf=" <> showFloat conf <> ")"

-- | Format float to 2 decimal places
showFloat :: Float -> Text
showFloat f = 
  let rounded = fromIntegral (round (f * 100) :: Int) / 100 :: Float
  in T.pack (show rounded)

-- | Current recognized strategy (if confident > 0.75)
recognizedStrategy :: StrategyBelief -> Maybe (EnemyStrategy, Float)
recognizedStrategy belief =
  case sbRecognized belief of
    Just s -> Just (s, sbConfidence belief)
    Nothing -> Nothing

-- | All posterior probabilities
allPosteriors :: StrategyBelief -> HashMap EnemyStrategy Float
allPosteriors = sbPosterior

-- | Diagnostic trace for analysis
diagnosticTrace :: StrategyBelief -> [Text]
diagnosticTrace = sbDiagnosticTrace
