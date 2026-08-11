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
  , getNexus
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
import Data.Function (on)
import Data.List (find, sortBy)
import Data.Maybe (mapMaybe)
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

canAfford :: (HasObs d, HasReservedCost d) => UnitTypeId -> StepMonad d Bool
canAfford = agentCanAfford

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

findPlacementPos :: Observation -> [TilePos] -> Grid -> Grid -> UnitTypeId -> Maybe TilePos
findPlacementPos _ expands grid gridHeight ProtossNexus = find (\pos -> canPlaceBuilding grid gridHeight pos (getFootprint ProtossNexus)) expands
findPlacementPos obs _ grid gridHeight ProtossPylon =
  findPlacementPoint grid gridHeight (getFootprint ProtossPylon) nexusPos (const True)
 where
  nexusPos = tilePos $ getNexus obs ^. #pos
findPlacementPos obs _ grid gridHeight uid = go pylons
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
findFreeGeyser obs = do
  let assimilatorPositions = Set.fromList $ runC $ unitsSelf obs .| unitTypeC ProtossAssimilator .| mapTilePosC
  nexus <- findNexus obs
  let nexusPos = tilePos $ nexus ^. #pos
      geysersSorted =
        sortBy
          (compare `on` (\u -> Spatial.distSquared u nexusPos))
          (runC $ obsUnitsC obs .| filterC isGeyser)
  find (\u -> not (tilePos (u ^. #pos) `Set.member` assimilatorPositions)) geysersSorted

findPlacementTarget :: (HasObs d, HasGrid d) => UnitTypeId -> StepMonad d (Maybe Target)
findPlacementTarget uid = do
  obs <- agentObs
  grid <- agentGrid
  si <- agentStatic
  pure $
    if uid == ProtossAssimilator
      then TargetUnit <$> findFreeGeyser obs
      else TargetPos <$> findPlacementPos obs (expandsPos si) grid (heightMap si) uid

-- ##################################### UNIT UTILS #####################################################################

-- TODO: now we check length 1 to filter out the
-- new assigned builder.
unitIsHarvesting :: Units.Unit -> Bool
unitIsHarvesting u = length orders == 1 && (HARVESTGATHERPROBE `elem` orders || HARVESTRETURNPROBE `elem` orders) -- `Utils.dbg` (show orders)
 where
  orders = toEnum' . view #abilityId <$> u ^. #orders

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
  isReturnsVespene = length orders == 1 && head orders == HARVESTRETURNPROBE && u ^. #vespeneContents > 0

getOverWorkersFrom :: [Units.Unit] -> [Units.Unit] -> [Units.Unit]
getOverWorkersFrom buildings workers = concatMap getFrom buildings
 where
  getFrom b
    | unitsToDrop > 0 = take (fromIntegral unitsToDrop) assignedWorkers
    | otherwise = []
   where
    unitsToDrop = b ^. #assignedHarvesters - b ^. #idealHarvesters
    assignedWorkers = filter (unitIsAssignedTo b) workers
