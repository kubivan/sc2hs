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
import Lens.Micro.Extras (view)
import Observation
  ( Cost
  , Observation
  , findNexus
  , obsResources
  , obsUnitsC
  , unitsSelf
  )
import Proto.S2clientprotocol.Raw qualified as R
import SC2.Grid
  ( Grid
  , TilePos
  , canPlaceBuilding
  , findPlacementPoint
  , findPlacementPointInRadius
  , tilePos
  )
import SC2.Ids.AbilityId
import SC2.Ids.UnitTypeId
import SC2.Spatial qualified as Spatial
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

import Conduit (filterC, mapC, (.|))
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List (find, sortBy)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Footprint (getFootprint)
import Safe (headMay)
import Target (Target (..))

findAssignee :: Observation -> Action -> Maybe Unit
findAssignee obs a = find (\u -> (u ^. #tag) `elem` [u ^. #tag | u <- getExecutors a]) (obs ^. (#rawData . #units))

actionCost :: StaticInfo -> Action -> Cost
actionCost si = unitCost (unitTraits si) . abilityToUnit (unitTraits si) . getCmd

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
findPlacementPos si _ _ grid gridHeight ProtossPylon =
  findPlacementPoint grid gridHeight (getFootprint ProtossPylon) (startLocation si) (const True)
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
          (\ug -> 100 >= Spatial.distSquared ug (minimumBy (comparing (Spatial.distSquared ug)) nexuses))

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

unitIsHarvesting :: Units.Unit -> Bool
unitIsHarvesting u = orders == Just HARVESTGATHERPROBE || orders == Just HARVESTRETURNPROBE
 where
  orders = listToMaybe $ toEnum' . view #abilityId <$> u ^. #orders

getTargetUnitTag :: Units.UnitOrder -> Maybe UnitTag
getTargetUnitTag unitOrder = case unitOrder ^. #maybe'target of
  Just (R.UnitOrder'TargetUnitTag tag) -> Just tag
  _ -> Nothing

unitIsAssignedTo :: Units.Unit -> Units.Unit -> Bool
unitIsAssignedTo building unit
  | isAssimilator building || isMineral building = building ^. #tag `elem` targets
  | toEnum' (building ^. #unitType) == ProtossNexus =
      unitIsHarvesting unit && closeEnough && withoutVespene
  | otherwise = error ("not implemented unitIsAssignedTo: " ++ show building)
 where
  targets = mapMaybe getTargetUnitTag (unit ^. #orders)
  closeEnough = Spatial.distManhattan building unit <= 12
  withoutVespene = unit ^. #vespeneContents == 0

unitIsAssignedToAny :: [Units.Unit] -> Units.Unit -> Bool
unitIsAssignedToAny buildings unit = any (`unitIsAssignedTo` unit) buildings

-- TODO: maybe the vespen & return check is enough
-- (probably units inside assimilators is not presented in the obs)
-- TODO: check if so: probes count between loops
unitIsVespeneHarvester :: [Units.Unit] -> Units.Unit -> Bool
unitIsVespeneHarvester assimilators u = unitIsAssignedToAny assimilators u || isReturnsVespene
 where
  orders = toEnum' . view #abilityId <$> u ^. #orders
  isReturnsVespene = listToMaybe orders == Just HARVESTRETURNPROBE && u ^. #vespeneContents > 0

getOverWorkersFrom :: [Units.Unit] -> [Units.Unit] -> [Units.Unit]
getOverWorkersFrom buildings workers = concatMap getFrom buildings
 where
  getFrom b
    | unitsToDrop > 0 = take (fromIntegral unitsToDrop) assignedWorkers
    | otherwise = []
   where
    unitsToDrop = b ^. #assignedHarvesters - b ^. #idealHarvesters
    assignedWorkers = filter (unitIsAssignedTo b) workers
