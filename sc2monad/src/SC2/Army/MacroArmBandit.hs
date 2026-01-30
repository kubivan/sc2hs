{-# LANGUAGE OverloadedStrings #-}

module SC2.Army.MacroArmBandit
  ( BanditArm (..)
  , BanditContext (..)
  , SimpleBandit (..)
  , emptySimpleBandit
  , selectArm
  , updateArm
  , armName
  , contextFromStrategicState
  ) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Hashable (Hashable, hashWithSalt)

-- | Macro-level arms: strategic levers for adaptation
data BanditArm
  = ArmExpand
  | ArmTechUp
  | ArmHarass
  | ArmAttackTiming
  | ArmDefensiveFortify
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | Context features for arm selection
data BanditContext = BanditContext
  { bcFriendlyPowerRatio     :: Float
  , bcMapControlFraction     :: Float
  , bcMineralBank            :: Float
  , bcGasBank                :: Float
  , bcEnemyMacroProb         :: Float
  , bcEnemyCheeseProb        :: Float
  , bcPhantomThreatNearBases :: Float
  , bcUpcomingTechScore      :: Float
  , bcExpansionCount         :: Float
  } deriving (Show)

-- | Simple bandit state: average reward per arm + visit count
-- (Simplified version without full linear model for now)
data SimpleBandit = SimpleBandit
  { sbArmRewards :: HashMap BanditArm Float
  , sbArmCounts  :: HashMap BanditArm Int
  , sbExplorationBonus :: Float
  } deriving (Show)

-- | Hashable instance for BanditArm
instance Hashable BanditArm where
  hashWithSalt s ArmExpand = hashWithSalt s (0 :: Int)
  hashWithSalt s ArmTechUp = hashWithSalt s (1 :: Int)
  hashWithSalt s ArmHarass = hashWithSalt s (2 :: Int)
  hashWithSalt s ArmAttackTiming = hashWithSalt s (3 :: Int)
  hashWithSalt s ArmDefensiveFortify = hashWithSalt s (4 :: Int)
emptySimpleBandit :: SimpleBandit
emptySimpleBandit =
  let arms = [minBound .. maxBound] :: [BanditArm]
  in SimpleBandit
       { sbArmRewards = HashMap.fromList [(arm, 0.0) | arm <- arms]
       , sbArmCounts = HashMap.fromList [(arm, 1) | arm <- arms]  -- +1 for regularization
       , sbExplorationBonus = 1.5
       }

-- | Select arm using UCB1
selectArm :: SimpleBandit -> BanditContext -> BanditArm
selectArm bandit _ =
  let arms = [minBound .. maxBound] :: [BanditArm]
      totalVisits = sum (HashMap.elems (sbArmCounts bandit))
      scores = [(arm, scoreArm bandit arm totalVisits) | arm <- arms]
      bestArm = fst $ maximumBy (comparing snd) scores
  in bestArm

-- | Compute UCB score for arm
scoreArm :: SimpleBandit -> BanditArm -> Int -> Double
scoreArm bandit arm totalVisits =
  let avgReward = realToFrac $ HashMap.findWithDefault 0.0 arm (sbArmRewards bandit) :: Double
      visits = fromIntegral $ HashMap.findWithDefault 1 arm (sbArmCounts bandit) :: Double
      bonus = realToFrac (sbExplorationBonus bandit) * sqrt (log (fromIntegral (max 1 totalVisits)) / visits)
  in avgReward + bonus

-- | Update arm with observed reward
updateArm :: BanditArm -> Float -> SimpleBandit -> SimpleBandit
updateArm arm reward bandit =
  let oldReward = HashMap.findWithDefault 0.0 arm (sbArmRewards bandit)
      oldCount = HashMap.findWithDefault 1 arm (sbArmCounts bandit)
      newReward = (oldReward * fromIntegral oldCount + reward) / fromIntegral (oldCount + 1)
  in bandit
       { sbArmRewards = HashMap.insert arm newReward (sbArmRewards bandit)
       , sbArmCounts = HashMap.insert arm (oldCount + 1) (sbArmCounts bandit)
       }

-- | Arm display name
armName :: BanditArm -> String
armName ArmExpand = "Expand"
armName ArmTechUp = "TechUp"
armName ArmHarass = "Harass"
armName ArmAttackTiming = "AttackTiming"
armName ArmDefensiveFortify = "DefensiveFortify"

-- | Extract context from components
contextFromStrategicState
  :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
  -> BanditContext
contextFromStrategicState fp ep mc mb gb em ec pt ts ec' =
  BanditContext
    { bcFriendlyPowerRatio = if ep > 0 then fp / ep else 1.0
    , bcMapControlFraction = mc
    , bcMineralBank = mb
    , bcGasBank = gb
    , bcEnemyMacroProb = em
    , bcEnemyCheeseProb = ec
    , bcPhantomThreatNearBases = pt
    , bcUpcomingTechScore = ts
    , bcExpansionCount = ec'
    }
