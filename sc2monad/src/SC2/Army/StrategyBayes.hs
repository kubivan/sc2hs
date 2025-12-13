{-# LANGUAGE OverloadedStrings #-}

module SC2.Army.StrategyBayes
  ( EnemyStrategy (..)
  , StrategySignature (..)
  , StrategyWeights (..)
  , StrategyBayes (..)
  , emptyStrategyBayes
  , updateStrategyBayes
  , bestStrategy
  , strategyConfidence
  , strategyEntropy
  ) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Hashable (Hashable, hashWithSalt)

-- | Enemy strategic archetypes recognized via Bayesian inference
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

-- | Template for recognizing a strategy from observations
data StrategySignature = StrategySignature
  { ssName            :: Text
  , ssStrategy        :: EnemyStrategy
  , ssKeyStructures   :: [Int]  -- UnitTypeId list
  , ssEarlyUnits      :: [Int]
  , ssProxyAllowed    :: Bool
  , ssExpansionEarlyFrame :: Maybe Int  -- expected expansion frame for Macro, Nothing if not key
  , ssGasBuildFrame   :: Maybe Int
  , ssWeights         :: StrategyWeights
  } deriving (Show)

-- | Likelihood weights for tuning strategy event scoring
data StrategyWeights = StrategyWeights
  { swStructureIn :: Float      -- ^ factor when structure observed in expected window
  , swStructureOut :: Float     -- ^ factor when structure observed outside window
  , swStructureMissed :: Float  -- ^ penalty for missed key structure after grace period
  , swUnitEarlyIn :: Float      -- ^ factor for early unit in window
  , swUnitEarlyOut :: Float     -- ^ factor for early unit outside window
  , swProxyDetected :: Float    -- ^ positive factor if proxy allowed, negative otherwise
  } deriving (Show)

-- | Posterior belief state over enemy strategies
data StrategyBayes = StrategyBayes
  { sbPrior     :: HashMap EnemyStrategy Float
  , sbLogLike   :: HashMap EnemyStrategy Float
  , sbPosterior :: HashMap EnemyStrategy Float
  , sbEntropy   :: Float
  , sbLastUpdate :: Int  -- game loop for smoothing
  } deriving (Show)

-- | Initialize uniform prior over standard strategies
emptyStrategyBayes :: StrategyBayes
emptyStrategyBayes = StrategyBayes
  { sbPrior = HashMap.fromList
      [ (Macro, 0.4)
      , (Cheese, 0.3)
      , (TimingRush, 0.2)
      , (BuildOrder, 0.1)
      ]
  , sbLogLike = HashMap.fromList
      [ (Macro, 0.0)
      , (Cheese, 0.0)
      , (TimingRush, 0.0)
      , (BuildOrder, 0.0)
      ]
  , sbPosterior = HashMap.fromList
      [ (Macro, 0.25)
      , (Cheese, 0.25)
      , (TimingRush, 0.25)
      , (BuildOrder, 0.25)
      ]
  , sbEntropy = 1.386  -- log(4) for uniform
  , sbLastUpdate = 0
  }

-- | Event types that inform strategy likelihood
data StrategyEvent
  = EvStructureBuilt Int Int        -- ^ (UnitTypeId, frameObserved)
  | EvUnitProduced Int Int          -- ^ (UnitTypeId, frameObserved)
  | EvExpansionBuilt Int            -- ^ frameObserved
  | EvProxyStructureDetected        -- ^ near enemy base
  | EvEarlyGasBuilt Int             -- ^ frameObserved
  deriving (Show)

-- | Score likelihood of a single event under a strategy
eventLikelihood :: StrategyEvent -> EnemyStrategy -> StrategySignature -> Float
eventLikelihood ev strat sig
  | strat /= ssStrategy sig = 1.0  -- neutral for unrelated strategies
  | otherwise = case ev of
      EvStructureBuilt uid frame ->
        let weights = ssWeights sig
            inWindow = case ssWeights sig of
              w -> True  -- simplified; in full version check timing window
        in if inWindow
             then swStructureIn weights
             else swStructureOut weights
      
      EvUnitProduced uid frame ->
        let weights = ssWeights sig
        in if uid `elem` ssEarlyUnits sig
             then swUnitEarlyIn weights
             else 0.8
      
      EvExpansionBuilt frame ->
        if strat == Macro
          then case ssExpansionEarlyFrame sig of
                 Just win -> if frame <= win + 500 then 1.5 else 0.5
                 Nothing -> 1.0
          else 0.2
      
      EvProxyStructureDetected ->
        let weights = ssWeights sig
        in if ssProxyAllowed sig then swProxyDetected weights else 0.1
      
      EvEarlyGasBuilt frame ->
        if strat == Macro
          then case ssGasBuildFrame sig of
                 Just win -> if frame >= win then 1.2 else 0.9
                 Nothing -> 1.0
          else 0.4

-- | Update posterior given observed events
-- Simplified: apply log-likelihood deltas and renormalize with smoothing
updateStrategyBayes
  :: [StrategyEvent]
  -> [StrategySignature]
  -> Float  -- ^ smoothing factor (0.0 = ignore old, 1.0 = keep old entirely)
  -> StrategyBayes
  -> StrategyBayes
updateStrategyBayes events sigs smoothFactor bayes =
  let strategies = [Macro, Cheese, TimingRush, BuildOrder]
      -- Accumulate log-likelihood deltas per strategy
      deltas = HashMap.fromList
        [ (s, sum [logFactorOf ev s | ev <- events])
        | s <- strategies
        ]
      -- Apply smoothing: blend old log-likelihood with new delta
      newLogLike = HashMap.intersectionWith blendLike (sbLogLike bayes) deltas
      -- Compute unnormalized posterior
      priorMap = sbPrior bayes
      unnormalized = HashMap.intersectionWith
        (\p ll -> p * exp ll)
        priorMap
        newLogLike
      z = sum unnormalized
      newPosterior = if z > 0 then HashMap.map (/ z) unnormalized else priorMap
      -- Compute entropy
      entropy = computeEntropy newPosterior
  in StrategyBayes
       { sbPrior = sbPrior bayes
       , sbLogLike = newLogLike
       , sbPosterior = newPosterior
       , sbEntropy = entropy
       , sbLastUpdate = sbLastUpdate bayes + 1
       }
  where
    blendLike oldLike delta =
      smoothFactor * oldLike + (1.0 - smoothFactor) * delta
    
    logFactorOf ev s =
      log $ maximum
        [ eventLikelihood ev s sig
        | sig <- sigs
        , ssStrategy sig == s
        ]

-- | Best strategy and its posterior probability
bestStrategy :: StrategyBayes -> Maybe (EnemyStrategy, Float)
bestStrategy bayes =
  if HashMap.null (sbPosterior bayes)
    then Nothing
    else let (s, p) = maximumBy (comparing snd) (HashMap.toList (sbPosterior bayes))
         in if p > 0.15 then Just (s, p) else Nothing

-- | Confidence level in inferred strategy (0=uncertain, 1=certain)
strategyConfidence :: StrategyBayes -> Float
strategyConfidence bayes =
  case bestStrategy bayes of
    Nothing -> 0.0
    Just (_, p) -> p  -- simple posterior mass as confidence

-- | Shannon entropy of posterior (0=certain, ~1.39=uniform over 4 strategies)
strategyEntropy :: StrategyBayes -> Float
strategyEntropy = sbEntropy

-- | Compute Shannon entropy
computeEntropy :: HashMap EnemyStrategy Float -> Float
computeEntropy post =
  negate $ sum
    [ p * log p
    | p <- HashMap.elems post
    , p > 0
    ]

-- Default strategy weights for tuning
defaultStrategyWeights :: StrategyWeights
defaultStrategyWeights = StrategyWeights
  { swStructureIn = 1.4
  , swStructureOut = 0.6
  , swStructureMissed = 0.3
  , swUnitEarlyIn = 1.5
  , swUnitEarlyOut = 0.5
  , swProxyDetected = 2.0
  }
