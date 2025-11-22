{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module SC2.Squad.Targeting (
    scoreTarget,
    findBestTargetTag,
) where

import Conduit (filterC, (.|))
import Data.List (maximumBy)
import Data.Function (on)
import Lens.Micro

import SC2.Proto.Data (Unit, UnitTypeData)
import SC2.Units.CombatEval (effectiveHp, unitDps, unitRange)
import StepMonad
import StepMonadUtils (siUnitData)
import Units (runC)
import SC2.Utils (isEnemy)
import SC2.Geometry (distSquared)
import Observation (obsUnitsC)
import Actions (UnitTag)

-- | Composite floating score for attacker targeting target.
-- Components:
--   In-range factor: favor targets already within weapon range.
--   Pickoff factor: inverse of effective HP (easier to kill fast).
--   Value factor: mineral+vespene+tech tier heuristic.
--   Threat factor: relative DPS ratio.
scoreTarget :: AgentDynamicState d => Unit -> Unit -> StepMonad d Float
scoreTarget attacker target = do
    targetData <- siUnitData target
    aRange <- unitRange attacker
    let dist2 = distSquared (attacker ^. #pos) (target ^. #pos)
        inRangeFactor = if dist2 <= aRange * aRange then 1.0 else aRange * aRange / (dist2 + 1e-3)
        ehp = effectiveHp target
        pickoffFactor = 1.0 / (ehp + 1.0)
        valueFactor = unitValue targetData
    aDps <- unitDps attacker
    tDps <- unitDps target
    let threatFactor = min 2.0 (tDps / (aDps + 0.01))
    return $ inRangeFactor * (pickoffFactor * 1.5 + valueFactor * 0.6 + threatFactor * 0.9)

-- Heuristic unit economic / tech value.
unitValue :: UnitTypeData -> Float
unitValue utd = mineral + vespene + buildTime * 0.5 where
    mineral   = fromIntegral (utd ^. #mineralCost)
    vespene   = 1.4 * fromIntegral (utd ^. #vespeneCost)
    buildTime = utd ^. #buildTime

-- | Returns best target tag among enemies near attacker; keeps currentTag if no better candidate found.
findBestTargetTag :: AgentDynamicState d => Unit -> UnitTag -> StepMonad d UnitTag
findBestTargetTag attacker currentTag = do
    obs <- agentObs
    let enemies = runC $ obsUnitsC obs .| filterC isEnemy
    scores <- mapM (\e -> do s <- scoreTarget attacker e; return (e, s)) enemies
    let betterThanCurrent = case lookupByTag currentTag scores of
            Just curScore -> filter (\(_,s) -> s > curScore * 1.05) scores
            Nothing -> scores
    return $ case betterThanCurrent of
        [] -> currentTag
        _  -> let (bestUnit,_) = maximumBy (compare `on` snd) betterThanCurrent in bestUnit ^. #tag

lookupByTag :: UnitTag -> [(Unit, Float)] -> Maybe Float
lookupByTag t = foldr (\(u,s) acc -> if u ^. #tag == t then Just s else acc) Nothing