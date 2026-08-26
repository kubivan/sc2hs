{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AgentBulidUtils where

import Actions
import Lens.Micro ((^.))
import Observation
  ( Cost
  , Observation
  , obsUnitsC
  , unitsSelf
  )
import Proto.S2clientprotocol.Raw qualified as R
import SC2.Grid
  ( Grid
  , canPlaceBuilding
  , findPlacementPoint
  , findPlacementPointInRadius
  )
import SC2.TilePos
import SC2.Ids.AbilityId
import SC2.Ids.UnitTypeId
import SC2.Spatial
import SC2.TechTree
import StepMonad
import StepMonadUtils (agentCanAfford, unitCost)
import Units
  ( Unit
  , UnitOrder
  , isAssimilator
  , isGeyser
  , isMineral
  , mapTilePosC
  , runC
  , toEnum'
  , unitTypeC
  )
import Utils

import Conduit (filterC, mapC, (.|))
import Data.Foldable (minimumBy)
import Data.Foldable.Extra (asum)
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Footprint (getFootprint)
import Target (Target (..))

findAssignee :: Observation -> Action -> Maybe Unit
findAssignee obs a = find (\u -> (u ^. #tag) `elem` [u ^. #tag | u <- getExecutors a]) (obs ^. (#rawData . #units))

actionCost :: StaticInfo -> Action -> Cost
actionCost si = unitCost (unitTraits si) . abilityToUnit (unitTraits si) . getCmd

actionCostSafe :: StaticInfo -> Action -> Maybe Cost
actionCostSafe si act = unitCost (unitTraits si) <$> (abilityToUnitSafe (unitTraits si) . getCmd $ act)

actionsCost :: StaticInfo -> [Action] -> Cost
actionsCost si xs = sum $ actionCost si <$> xs

agentFindBuilder :: (HasObs d) => StepMonad d (Maybe Unit)
agentFindBuilder = findBuilder <$> agentObs

findBuilder :: Observation -> Maybe Unit
findBuilder obs =
  find availableProbe (runC $ unitsSelf obs .| unitTypeC ProtossProbe)
 where
  availableProbe :: Unit -> Bool
  availableProbe unit =
    Prelude.null (unit ^. #orders)
      || ( length (unit ^. #orders) == 1
             && HARVESTGATHERPROBE `elem` map (toEnum' . (^. #abilityId)) (unit ^. #orders)
         )

pylonRadius :: Float
pylonRadius = 6.5

findPlacementPos ::
  StaticInfo -> Observation -> [TilePos] -> Grid -> Grid -> UnitTypeId -> Maybe TilePos
findPlacementPos _ _ expands grid gridHeight ProtossNexus = find (\pos -> canPlaceBuilding grid gridHeight pos (getFootprint ProtossNexus)) expands
findPlacementPos si obs _ grid gridHeight ProtossPylon =
  asum
    [ findPlacementPoint grid gridHeight (getFootprint ProtossPylon) nexusPos distantEnough
    | -- TODO: currenty startpos + all nexuses is add first nexus pos twice
    nexusPos <- startLocation si : runC (unitsSelf obs .| unitTypeC ProtossNexus .| mapTilePosC)
    ]
 where
  pylons = runC (unitsSelf obs .| unitTypeC ProtossPylon .| mapTilePosC)
  distantEnough pos = all (\p -> distSquaredI pos p >= 3 * 3) pylons
findPlacementPos _ obs _ grid gridHeight uid = go pylons
 where
  go (p : ps) =
    case findPlacementPointInRadius grid gridHeight (getFootprint uid) p pylonRadius of
      Just res -> Just res
      Nothing -> go ps
  go [] = Nothing
  pylons =
    runC $
      unitsSelf obs
        .| unitTypeC ProtossPylon
        .| mapTilePosC

findFreeGeyser :: Observation -> Maybe Unit
findFreeGeyser obs = listToMaybe freeGeysers
 where
  assimilatorPositions = Set.fromList $ runC $ unitsSelf obs .| unitTypeC ProtossAssimilator .| mapTilePosC
  nexuses = runC $ unitsSelf obs .| unitTypeC ProtossNexus .| mapTilePosC
  freeGeysers =
    runC $
      obsUnitsC obs
        .| filterC isGeyser
        .| filterC (\u -> not (tilePos (u ^. #pos) `Set.member` assimilatorPositions)) -- not occupied
        .| filterC
          (\ug -> 100 >= distSquaredI ug (minimumBy (comparing (distSquaredI ug)) nexuses))

findPlacementTarget :: (HasObs d, HasGrid d) => UnitTypeId -> StepMonad d (Maybe Target)
findPlacementTarget uid = do
  obs <- agentObs
  grid <- agentGrid
  si <- agentStatic
  pure $
    if uid == ProtossAssimilator
      then TargetUnit <$> findFreeGeyser obs
      else TargetPos <$> findPlacementPos si obs (expandsPos si) grid (heightMap si) uid

-- ##################################### UNIT UTILS #####################################################################

getOverWorkersFrom :: [Units.Unit] -> [Units.Unit] -> [Units.Unit]
getOverWorkersFrom buildings workers = concatMap getFrom buildings
 where
  getFrom b
    | unitsToDrop > 0 = take (fromIntegral unitsToDrop) assignedWorkers
    | otherwise = []
   where
    unitsToDrop = b ^. #assignedHarvesters - b ^. #idealHarvesters
    assignedWorkers = filter (unitIsAssignedTo b) workers
