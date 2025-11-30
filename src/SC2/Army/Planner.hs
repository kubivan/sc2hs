module SC2.Army.Planner (
    AIMode(..)
  , ArmyIntent(..)
  , computeArmyPlan
  , currentArmyIntent
) where

import StepMonad (StepMonad, agentObs, AgentDynamicState)
import Observation (obsUnitsC)
import Units (runC, allianceC)
import Conduit (filterC, (.|))
import SC2.Proto.Data (Alliance(..))
import SC2.Army.ThreatMap (computeThreatMap, regionThreat, ThreatMap(..))
import SC2.Grid.Algo (RegionId)
import SC2.Utils (isEnemy)
import Data.HashMap.Strict qualified as HashMap
import Data.List (maximumBy)
import Data.Ord (comparing)

-- | High-level army mode decided by coarse evaluation of forces.
-- This will later incorporate threat maps, tech timing and strategic goals.
-- For now it's a placeholder using simple unit counts.

data AIMode = AimAttack | AimDefend | AimHarass | AimRegroup
  deriving (Show, Eq)

data ArmyIntent = ArmyIntent { aiMode :: AIMode, aiFocusRegion :: Maybe Int }
  deriving (Show, Eq)

-- | Compute the current army intent.
-- Placeholder logic:
--  Attack    if friendly units > 1.2 * enemy units
--  Defend    if enemy units    > 1.2 * friendly units
--  Regroup   otherwise
--  Harass    not yet selected by heuristic (reserved for future)
computeArmyPlan :: AgentDynamicState d => StepMonad d ArmyIntent
computeArmyPlan = do
  obs <- agentObs
  let friends = runC $ obsUnitsC obs .| allianceC Self
      enemies = runC $ obsUnitsC obs .| allianceC Enemy
      fc = fromIntegral (length friends) :: Float
      ec = fromIntegral (length enemies) :: Float
      mode | fc > ec * 1.2 = AimAttack
           | ec > fc * 1.2 = AimDefend
           | otherwise     = AimRegroup
  tm <- computeThreatMap
  -- pick the region with max threat as focus if present
  let regionList = HashMap.toList (tmRegionThreat tm)
  let focus = if null regionList then Nothing else Just (fst $ maximumBy (comparing snd) regionList)
  pure $ ArmyIntent mode focus

-- | Convenience wrapper for squads to read the current army intent.
currentArmyIntent :: AgentDynamicState d => StepMonad d ArmyIntent
currentArmyIntent = computeArmyPlan
