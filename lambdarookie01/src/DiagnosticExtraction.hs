{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}

module DiagnosticExtraction where

import Observation
import SC2.Army.StrategyDetection (DiagnosticEvent (..))
import SC2.Geometry (distSquared)
import SC2.Grid.TilePos (tilePos)
import Units (Unit, unitTypeC, runC, toEnum', isBuilding, (.|))
import SC2.Ids.UnitTypeId
import SC2.Proto.Data (Alliance(..))
import Lens.Micro ((^.))
import Conduit (filterC, headC, mapC, runConduitPure, sinkList, (.|))
import Data.Maybe (listToMaybe)

-- | Extract diagnostic events from current and previous observations
extractDiagnosticEvents :: Observation -> Observation -> [DiagnosticEvent]
extractDiagnosticEvents obs obsPrev = concat
  [ detectProxyStructure obs
  , detectEarlyExpansion obs obsPrev
  , detectTechBuilding obs
  ]

-- | Detect if enemy has a pylon/structure near our base
-- This is a high-confidence indicator of a proxy rush or timing pressure
detectProxyStructure :: Observation -> [DiagnosticEvent]
detectProxyStructure obs =
  case findNexusOpt obs of
    Nothing -> []
    Just nexusPos ->
      let enemyStructures = runConduitPure (obsUnitsC obs .| filterC isEnemyBuilding .| mapC (\u -> (u ^. #pos, u ^. #unitType)) .| sinkList)
          -- Within ~20 tiles of our base
          closeStructures = filter (\(pos, _) -> distSquared nexusPos pos < 400) enemyStructures
       in if not (null closeStructures)
            then [ProxyStructureNearBase]
            else []
  where
    findNexusOpt obs =
      listToMaybe $ runConduitPure $
        unitsSelf obs
          .| unitTypeC ProtossNexus
          .| mapC (^. #pos)
          .| sinkList

-- | Detect early expansion (e.g., enemy nexus or production before expected time)
detectEarlyExpansion :: Observation -> Observation -> [DiagnosticEvent]
detectEarlyExpansion obs obsPrev =
  let gameLoop = obs ^. #gameLoop
      expansionThreshold = 2000  -- Before ~2000 frames (~90 seconds)
      -- Count enemy production structures (not main base)
      enemyProduction = runConduitPure $
        obsUnitsC obs
          .| filterC isEnemyBuilding
          .| filterC isProduction
          .| sinkList
      prevEnemyProduction = runConduitPure $
        obsUnitsC obsPrev
          .| filterC isEnemyBuilding
          .| filterC isProduction
          .| sinkList
     in if (fromIntegral gameLoop :: Int) < expansionThreshold && length enemyProduction > length prevEnemyProduction
       then [EarlyExpansion (fromIntegral gameLoop)]
        else []
  where
    isProduction u = toEnum' (u ^. #unitType) `elem` [ProtossNexus, ProtossGateway, ProtossDarkShrine]

-- | Detect tech buildings (e.g., robo, stargate, twilight)
detectTechBuilding :: Observation -> [DiagnosticEvent]
detectTechBuilding obs =
  let techBuildings = runConduitPure $
        obsUnitsC obs
          .| filterC isEnemyBuilding
          .| filterC isTechBuilding
          .| sinkList
     in if not (null techBuildings)
       then [EnemyTechBuilding (length techBuildings)]
        else []
  where
    isTechBuilding u = toEnum' (u ^. #unitType) `elem`
      [ProtossRoboticsFacility, ProtossStargate, ProtossTwilightCouncil]

isEnemyBuilding :: Unit -> Bool
isEnemyBuilding u = (u ^. #alliance == Enemy) && isBuilding u
