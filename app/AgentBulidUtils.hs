{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AgentBulidUtils where

import AbilityId
import Actions
import Agent (
    AgentDynamicState (..),
    StaticInfo (unitTraits),
    StepMonad,
    UnitTraits,
    agentObs,
    agentStatic,
 )
import Conduit (filterC, mapC, (.|))
import Data.Function (on)
import Data.HashMap.Strict qualified as HashMap
import Data.List (find, sortBy)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Footprint (getFootprint)
import Grid.Grid (
    Grid (..),
    canPlaceBuilding,
    findPlacementPoint,
    findPlacementPointInRadius,
 )
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)
import Observation (
    Cost (Cost),
    Observation,
    findNexus,
    obsUnitsC,
    unitsSelf,
 )
import Proto.S2clientprotocol.Raw qualified as R

import Safe (headMay)
import UnitTypeId
import Units (
    Unit,
    UnitOrder,
    isAssimilator,
    isGeyser,
    isMineral,
    mapTilePosC,
    runC,
    toEnum',
    unitTypeC,
 )
import Utils (
    TilePos,
    distManhattan,
    distSquared,
    tilePos,
 )

-- TODO: move to UnitTraits
abilityToUnit :: Agent.UnitTraits -> AbilityId -> UnitTypeId
abilityToUnit traits a = case find (\x -> fromIntegral (x ^. #abilityId) == fromEnum a) (HashMap.elems traits) of
    Just t -> toEnum . fromIntegral $ t ^. #unitId
    Nothing -> error $ "abilityToUnit: invalid ability: " ++ show a

-- TODO: move to UnitTraits
unitToAbility :: Agent.UnitTraits -> UnitTypeId -> AbilityId
unitToAbility traits uid = case traits HashMap.!? uid of
    Just t -> toEnum . fromIntegral $ t ^. #abilityId
    Nothing -> error $ "unitToAbility: invalid id: " ++ show uid

findAssignee :: Observation -> Action -> Maybe Units.Unit
findAssignee obs a = find (\u -> (u ^. #tag) `elem` [u ^. #tag | u <- getExecutors a]) (obs ^. (#rawData . #units))

unitCost :: Agent.UnitTraits -> UnitTypeId -> Cost
unitCost traits uid = case traits HashMap.!? uid of
    Just t -> Cost (fromIntegral $ t ^. #mineralCost) (fromIntegral $ t ^. #vespeneCost)
    Nothing -> Cost 0 0

actionCost :: Agent.StaticInfo -> Action -> Cost
actionCost si = unitCost (unitTraits si) . abilityToUnit (unitTraits si) . getCmd

actionsCost :: Agent.StaticInfo -> [Action] -> Cost
actionsCost si xs = sum $ actionCost si <$> xs

canAfford :: (Agent.AgentDynamicState d) => UnitTypeId -> Cost -> Agent.StepMonad d (Bool, Cost)
canAfford uid r = do
    si <- Agent.agentStatic
    obs <- Agent.agentObs

    let minerals = fromIntegral $ obs ^. (#playerCommon . #minerals)
        vespene = fromIntegral $ obs ^. (#playerCommon . #vespene)
        resources = Cost minerals vespene
        cost = unitCost (unitTraits si) uid
    return (resources >= cost + r, cost) -- `Utils.dbg` ("minerals: " ++ show minerals)

findBuilder :: Observation -> Maybe R.Unit
findBuilder obs =
    headMay $
        runC $
            unitsSelf obs
                .| unitTypeC ProtossProbe
                -- .| unitIdleC
                -- .| filterC unitIsHarvesting
                .| filterC
                    ( \x ->
                        Prelude.null (x ^. #orders)
                            || (length (x ^. #orders) == 1 && HarvestGatherProbe `elem` map (\o -> toEnum' (o ^. #abilityId)) (x ^. #orders)) -- TODO: fix, add proper o
                    )

pylonRadius :: Float
pylonRadius = 6.5

findPlacementPos :: Observation -> [TilePos] -> Grid -> Grid -> UnitTypeId -> Maybe TilePos
findPlacementPos _ expands grid heightMap ProtossNexus = find (\x -> canPlaceBuilding grid heightMap x (getFootprint ProtossNexus)) expands
findPlacementPos obs _ grid heightMap ProtossPylon = findPlacementPoint grid heightMap (getFootprint ProtossPylon) nexusPos (const True)
  where
    nexusPos = tilePos $ findNexus obs ^. #pos
findPlacementPos obs _ grid heightMap id = go pylons
  where
    go :: [TilePos] -> Maybe TilePos
    go (p : ps) = case findPlacementPointInRadius grid heightMap (getFootprint id) p pylonRadius of
        Just res -> Just res
        Nothing -> go ps
    go [] = Nothing
    pylons =
        runC $
            unitsSelf obs
                .| unitTypeC ProtossPylon
                .| filterC (\u -> u ^. #buildProgress == 1)
                .| mapC (\x -> tilePos $ x ^. #pos)

findFreeGeyser :: Observation -> Maybe Units.Unit
findFreeGeyser obs = find (\u -> not (tilePos (u ^. #pos) `Set.member` assimilatorsPosSet)) geysersSorted
  where
    assimilatorsPosSet = Set.fromList $ runC $ unitsSelf obs .| unitTypeC ProtossAssimilator .| mapTilePosC
    nexusPos = tilePos $ findNexus obs ^. #pos
    geysers = runC $ obsUnitsC obs .| filterC isGeyser
    geysersSorted = sortBy (compare `on` (\x -> (x ^. #pos) `distSquared` nexusPos)) geysers

-- ##################################### UNIT UTILS #####################################################################

-- TODO: now we check length 1 to filter out the
-- new assigned builder.
unitIsHarvesting :: Units.Unit -> Bool
unitIsHarvesting u = length orders == 1 && (HarvestGatherProbe `elem` orders || HarvestReturnProbe `elem` orders) -- `Utils.dbg` (show orders)
  where
    orders = toEnum' . view #abilityId <$> u ^. #orders

getTargetUnitTag :: Units.UnitOrder -> Maybe UnitTag
getTargetUnitTag unitOrder = case unitOrder ^. #maybe'target of
    Just (R.UnitOrder'TargetUnitTag tag) -> Just tag
    _ -> Nothing

unitIsAssignedTo :: Units.Unit -> Units.Unit -> Bool
unitIsAssignedTo building unit
    | isAssimilator building || isMineral building = building ^. #tag `elem` targets
    | toEnum' (building ^. #unitType) == ProtossNexus = unitIsHarvesting unit && closeEnough && withoutVespene
    | otherwise = error ("not implemented unitIsAssignedTo: " ++ show building)
  where
    targets = mapMaybe getTargetUnitTag (unit ^. #orders)
    closeEnough = distManhattan (building ^. #pos) (unit ^. #pos) <= 12
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
    isReturnsVespene = length orders == 1 && head orders == HarvestReturnProbe && u ^. #vespeneContents > 0

getOverWorkersFrom :: [Units.Unit] -> [Units.Unit] -> [Units.Unit]
getOverWorkersFrom buildings workers = concatMap getFrom buildings
  where
    getFrom b
        | unitsToDrop > 0 = take (fromIntegral unitsToDrop) assignedWorkers
        | otherwise = []
      where
        unitsToDrop = b ^. #assignedHarvesters - b ^. #idealHarvesters
        assignedWorkers = filter (unitIsAssignedTo b) workers