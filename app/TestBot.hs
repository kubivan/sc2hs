{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module TestBot (TestBot (..)) where

import AbilityId

import Actions
import Agent
import Observation
import Conduit
import Control.Applicative (Alternative (..))
import Control.Monad
import Control.Monad.Writer.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.ByteString (putStr)
import Data.ProtoLens (defMessage, Tag (Tag))
import Data.Maybe ( isJust, catMaybes, fromJust, isNothing )
import Data.ByteString.Char8 (putStrLn)
import qualified Data.Conduit.List as CL
import Data.Foldable (toList)
import Data.HashMap.Strict qualified as HashMap
import Data.List (find, mapAccumL, sortBy, isPrefixOf, isSuffixOf, maximumBy, minimumBy)
import Data.Function (on)
import Data.Maybe
import Data.Sequence (Seq (..), empty, (|>))
import Data.String
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Char (ord)
import Data.Vector qualified as V
import Footprint
import GHC.Real (fromIntegral)
import GHC.Word qualified
import Grid (Grid(..), addMark, findPlacementPoint, findPlacementPointInRadius, printGrid, gridToFile, gridToStr, gridFromImage, gridPixel, canPlaceBuilding, gridPlace)
import Lens.Micro (to, (&), (.~), (^.), (^..), filtered, (%~))
import Lens.Micro.Extras(view)
import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import Proto.S2clientprotocol.Debug_Fields (pos, unitTag, unitType)
import Proto.S2clientprotocol.Query_Fields (abilities)
import Proto.S2clientprotocol.Raw as R
import Proto.S2clientprotocol.Raw_Fields as R
  ( abilityId,
    alliance,
    buildProgress,
    pos,
    startLocations,
    unitType,
    units,
  )
import Proto.S2clientprotocol.Sc2api qualified as A
import Proto.S2clientprotocol.Sc2api_Fields (minerals, step, vespene, gameLoop)
import Proto.S2clientprotocol.Sc2api_Fields qualified as A
import UnitTypeId
import Utils
import Safe (headMay)
import Debug.Trace
import Units
import Units(mapTilePosC, closestC, unitIdleC)
import Utils (distSquared, tilePos, distSquaredTile)
import Proto.S2clientprotocol.Common (Point)
import UnitTypeId (UnitTypeId(NeutralVespenegeyser, NeutralRichvespenegeyser, NeutralProtossvespenegeyser, NeutralPurifiervespenegeyser, NeutralShakurasvespenegeyser, ProtossNexus, ProtossGateway, ProtossRoboticsfacility, ProtossAssimilator))
import Data.Conduit.List (sourceList, consume)

type BuildOrder = [UnitTypeId]

data TestBot
  = Opening
  | BuildOrderExecutor BuildOrder [Action] Observation UnitAbilities

findAssignee :: Observation -> Action -> Maybe Units.Unit
findAssignee obs a = find (\u -> u ^. #tag == (getExecutor a) ^. #tag) (obs ^. (#rawData . #units))

abilityToUnit :: UnitTraits -> AbilityId -> UnitTypeId
abilityToUnit traits a = case find (\x -> fromIntegral (x ^. #abilityId) == fromEnum a) (HashMap.elems traits) of
  Just t -> toEnum . fromIntegral $ t ^. #unitId
  Nothing -> error $ "abilityToUnit: invalid ability: " ++ show a

unitToAbility :: UnitTraits -> UnitTypeId -> AbilityId
unitToAbility traits id = case traits HashMap.!? id of
  Just t -> toEnum . fromIntegral $ t ^. #abilityId
  Nothing -> error $ "unitToAbility: invalid id: " ++ show id

unitCost :: UnitTraits -> UnitTypeId -> Cost
unitCost traits id = case traits HashMap.!? id of
  Just t -> Cost (fromIntegral $ t ^. #mineralCost) (fromIntegral $ t ^. #vespeneCost)
  Nothing -> Cost 0 0

actionCost :: StaticInfo -> Action -> Cost
actionCost si = unitCost (unitTraits si) . abilityToUnit (unitTraits si) . getCmd

actionsCost :: StaticInfo -> [Action] -> Cost
actionsCost si xs = sum $ actionCost si <$> xs

canAfford :: UnitTypeId -> Cost -> StepMonad (Bool, Cost)
canAfford id r = do
  si <- agentStatic
  (obs, _) <- agentGet

  let minerals = fromIntegral $ obs ^. (#playerCommon . #minerals) -- `Utils.debug` ("minerals: " ++ show minerals)
      vespene = fromIntegral $ obs ^. (#playerCommon . #vespene)
      resources = Cost minerals vespene
      cost = unitCost (unitTraits si) id
  return (resources >= cost + r, cost)

inBuildThechTree :: UnitTypeId -> StepMonad Bool
inBuildThechTree id = do
  abilities <- agentAbilities
  si <- agentStatic
  let ability = unitToAbility (unitTraits si) id --`Utils.dbg` ("abilities: " ++ show abilities)
  return $ ability `elem` (abilities HashMap.! ProtossProbe)

findBuilder :: Observation -> Maybe R.Unit
findBuilder obs =
  headMay $
    runC $
      unitsSelf obs
        .| unitTypeC ProtossProbe
        .| filterC (\x -> Prelude.null (x ^. #orders) || (length (x ^. #orders) == 1 && HarvestGatherProbe `elem` map (\o -> toEnum' (o ^. #abilityId)) (x ^. #orders))) -- TODO: fix, add proper orders check

pylonRadius :: Float
pylonRadius = 6.5

findPlacementPos :: Observation -> [TilePos] -> Grid -> Grid -> UnitTypeId -> Maybe TilePos
findPlacementPos obs expands grid heightMap ProtossNexus = find (\x -> canPlaceBuilding grid heightMap x (getFootprint ProtossNexus)) expands
  where
    nexusesSet = Set.fromList $ runC $
      obsUnitsC obs .| filterC (\x -> toEnum' (x ^. #unitType) `notElem` [ProtossNexus, TerranCommandcenter, ZergHatchery]) .| mapTilePosC

findPlacementPos obs expands grid heightMap ProtossPylon = findPlacementPoint grid heightMap (getFootprint ProtossPylon) nexusPos (const True)
  where
    nexusPos = tilePos $ findNexus obs ^. #pos

findPlacementPos obs expands grid heightMap id = go pylons
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

findFreeGeyser obs = find (\u -> not (tilePos (u ^. #pos) `Set.member` assimilatorsPosSet)) geysersSorted
  where
    assimilatorsPosSet = Set.fromList $ runC $ unitsSelf obs .| unitTypeC ProtossAssimilator .| mapTilePosC
    nexusPos = tilePos $ findNexus obs ^. #pos
    geysers = runC $ obsUnitsC obs .| filterC isGeyser
    geysersSorted = sortBy (compare `on` (\x -> tilePos (x ^. #pos) `distSquaredTile` nexusPos)) geysers

buildAction :: UnitTypeId -> Grid -> Cost -> MaybeStepMonad (Action, Cost, Grid)
buildAction ProtossAssimilator grid reservedRes = do
  (isAffordable, cost) <- lift $ canAfford ProtossAssimilator reservedRes
  guard isAffordable
  (obs, _) <- lift agentGet
  builder <- MaybeT . return $ findBuilder obs
  geyser <- MaybeT . return $ findFreeGeyser obs
  --TODO: now we don't need to place to built geysers
  --let footprint = getFootprint order
  --let grid' = addMark grid footprint pos
  --let obs' = addOrder (builder ^. #tag) ability . addUnit order $ obs
  --lift . agentPut $ (obs', grid)

  let res = UnitCommand BuildAssimilator builder geyser

  return (res, cost, grid)  `Utils.dbg` ("building Assimilator target " ++ show geyser ++ " builder " ++ show builder ++ " putting to the grid!!!!")--`Utils.dbg` ("builder orders "  ++ show (builder ^. #orders) )

buildAction order grid reservedRes = do
  enabled <- lift . inBuildThechTree $ order
  guard enabled --`Utils.dbg` (show order ++ " enabled " ++ show enabled)
  (isAffordable, cost) <- lift $ canAfford order reservedRes
  guard isAffordable

  si <- lift agentStatic
  (obs, _) <- lift agentGet
  let ability = unitToAbility (unitTraits si) order
  let footprint = getFootprint order
  guard (isBuildAbility ability)
  builder <- MaybeT . return $ findBuilder obs
  pos <- MaybeT . return $ findPlacementPos obs (expandsPos si) grid (heightMap si) order
  let grid' = addMark grid footprint pos
  let obs' = addOrder (builder ^. #tag) ability . addUnit order $ obs
  lift . agentPut $ (obs', grid) `Utils.dbg` (show order ++ " buildPos " ++ show pos ++ " builder " ++ show builder ++ " putting to the grid!!!!")

  let res = PointCommand ability builder (fromTuple pos)

  return (res, cost, grid') `Utils.dbg` ("builder orders "  ++ show (builder ^. #orders) )

distantEnough units radius pos = all (\p -> distSquaredTile pos p >= radius*radius) units

pylonBuildAction :: Grid -> Cost -> MaybeStepMonad (Action, Cost, Grid)
pylonBuildAction grid reservedRes = do
  (isAffordable, cost) <- lift $ canAfford ProtossPylon reservedRes
  guard isAffordable

  si <- lift agentStatic
  (obs, _) <- lift agentGet
  let hasPylonsInProgress = not $ Prelude.null $ runC $ unitsSelf obs .| unitTypeC ProtossPylon .| filterC (\u -> u ^. #buildProgress < 1)
  if hasPylonsInProgress
    then MaybeT $ return Nothing
    else do
      builder <- MaybeT . return $ findBuilder obs
      let nexus = findNexus obs
          footprint = getFootprint ProtossPylon
          findPylonPlacementPoint = findPlacementPoint grid (heightMap si) footprint (tilePos (builder ^. #pos))
          units = runC $ unitsSelf obs .| unitTypeC ProtossPylon .| mapTilePosC
          pylonCriteria = distantEnough units
      pylonPos <- MaybeT . return $ findPylonPlacementPoint (pylonCriteria pylonRadius)
        -- <|> findPylonPlacementPoint (pylonCriteria (pylonRadius / 2))
        -- <|> findPylonPlacementPoint (const True)

      let grid' = addMark grid footprint pylonPos
      let obs' = addOrder (builder ^. #tag) AbilityId.BuildPylon obs
      lift . agentPut $ (obs', grid')
      return (PointCommand BuildPylon builder (fromTuple pylonPos), cost, grid')

createAction :: Grid -> Cost -> UnitTypeId -> MaybeStepMonad (Action, Cost, Grid)
createAction grid reserved order = do
  (isAffordable, cost) <- lift $ canAfford order reserved
  guard isAffordable -- `Utils.dbg` (show order ++ " affordable " ++ show isAffordable ++ " cost: " ++ show cost)

  buildAction order grid reserved <|> pylonBuildAction grid reserved

trainProbes :: StepMonad ()
trainProbes = do
  obs <- agentObs
  let units = unitsSelf obs
      probeCount = runConduitPure $ units .| unitTypeC ProtossProbe .| lengthC
      assimCount = runConduitPure $ units .| unitTypeC ProtossAssimilator .| lengthC
      nexuses :: [Units.Unit]
      nexuses = runC $ units .| unitTypeC ProtossNexus .| filterC (\n -> (n ^. #buildProgress) == 1) .| unitIdleC
      nexusCount = length nexuses
      optimalCount = assimCount * 3 + nexusCount * 16
  when (optimalCount - probeCount > 0) $ command [SelfCommand TrainProbe n | n <- nexuses] `Utils.dbg` ("trainProbes: optCount" ++ show optimalCount ++ " probes: " ++ show probeCount )

reassignIdleProbes :: StepMonad ()
reassignIdleProbes = do
  obs <- agentObs
  let units = unitsSelf obs
      probes = units .| unitTypeC ProtossProbe
      mineralField = headMay $ runC $ probes
        .| filterC (\p -> HarvestGatherProbe `elem` map (\o -> toEnum' (o ^. #abilityId)) (p ^. #orders))
        .| mapC (\harvester -> head $ filter (\o -> HarvestGatherProbe == toEnum' (o ^. #abilityId)) (harvester ^. #orders))
        .| mapC (\harvestOrder -> harvestOrder ^. #targetUnitTag)
        .| mapC (getUnit obs) --TODO: should getUnit return maybe

      closestMineral to = runConduitPure $ obsUnitsC obs .| unitTypeC NeutralMineralfield .| closestC to

      idleWorkers = runC $ probes
        .| unitIdleC

  command [UnitCommand HarvestGatherProbe idle (fromJust $ mineralField <|> closestMineral idle ) | idle <- idleWorkers]

processQueue :: [Action] -> ([Action], [Action]) -> StepMonad ([Action], [Action])
processQueue (a : as) (q', interrupted) = do
  obs <- agentObs
  case findAssignee obs a of
    Nothing -> processQueue as (q', interrupted ++ [a])
    Just u ->
      if fromEnum (getCmd a) `elem` (u ^. #orders ^.. traverse . (A.abilityId . to fromIntegral))
        then processQueue as (q' ++ [a], interrupted)
        else processQueue as (q', interrupted)
processQueue [] res = return res

splitAffordable :: BuildOrder -> Cost -> StepMonad ([Action], BuildOrder)
splitAffordable bo reserved = agentGet >>=(\(_, grid) -> go bo Data.Sequence.empty grid reserved) -- `Utils.dbg` ("splitAffordable " ++ show bo ++ " reserved" ++ show reserved)
  where
    go :: BuildOrder -> Seq Action -> Grid -> Cost -> StepMonad ([Action], BuildOrder)
    go bo@(uid:remaining) acc grid reservedCost = do
      (si, _) <- agentAsk
      cres <- tryCreate grid reservedCost uid
      case cres of
        Just (action, cost, grid') -> go remainingOrBo (acc |> action) grid' (cost + reserved)
          where
            -- if we fallback to the BuildPylon, don't remove the order from BO
            remainingOrBo = if getCmd action == unitToAbility (unitTraits si) uid then remaining else bo

        Nothing -> return (toList acc, bo)
    go [] acc _ _ = return (toList acc, [])

    tryCreate :: Grid -> Cost -> UnitTypeId -> StepMonad (Maybe (Action, Cost, Grid))
    tryCreate grid reserved uid = runMaybeT $ createAction grid reserved uid

debugUnitPos = agentObs >>= \obs -> debugTexts [("upos " ++ show (c ^. #pos), c ^. #pos) | c <- runC $ unitsSelf obs]
  
agentUpdateGrid f = do
  (obs, grid) <- agentGet
  agentPut (obs, f grid)

instance Agent TestBot where
  agentDebug _ = return ()

  agentRace _ = C.Protoss
  agentStep Opening = do
    si <- agentStatic
    (obs, grid) <- agentGet
    let gi = gameInfo si
    agentPut (obs, gridUpdate obs grid)
    let nexus = findNexus obs
    let fourGate = [ProtossPylon, ProtossAssimilator, ProtossGateway, ProtossCyberneticscore, ProtossAssimilator, ProtossGateway]

    command [SelfCommand AbilityId.TrainProbe nexus]
    return $ BuildOrderExecutor fourGate [] obs (HashMap.fromList [])

  agentStep (BuildOrderExecutor buildOrder queue obsPrev abilitiesPrev) = do
    debugUnitPos
    reassignIdleProbes
    trainProbes
    si <- agentStatic
    (obs, _) <- agentGet
    abilities <- agentAbilities
    if unitsChanged obs obsPrev || abilities /= abilitiesPrev then do
      --command [Chat "unitsChanged !!!: "]
      agentPut (obs, gridUpdate obs (gridFromImage $ gameInfo si ^. (#startRaw . #placementGrid))) -- >> command [Chat $ pack "grid updated"]
      (queue', interruptedAbilities) <- processQueue queue ([], [])
      agentUpdateGrid (\g -> foldl (\ga (u, p) -> gridPlace u p ga) g [(abilityToUnit (unitTraits si) . getCmd $ a, tilePos . getTarget $ a) | a <- queue'])
      let reservedResources = actionsCost si queue'
      let interruptedOrders = abilityToUnit (unitTraits si) . getCmd <$> interruptedAbilities
      unless (null interruptedOrders) $
        agentChat ("interrupted: " ++ show interruptedOrders)

      (affordableActions, orders') <- splitAffordable (interruptedOrders ++ buildOrder) reservedResources

      unless (null affordableActions) $ do
       agentChat ("scheduling: " ++ show affordableActions `dbg` ("!!! affordable " ++ show affordableActions))

      debugTexts [("planned " ++ show (getCmd a), defMessage & #x .~ getTarget a ^. #x & #y .~ getTarget a ^. #y & #z .~ 10) | a <- affordableActions]
      command affordableActions
      if null orders' then -- restart build order
        --return $ BuildOrderExecutor (replicate 50 ProtossPhotoncannon) [] obs (HashMap.fromList [])
        return $ BuildOrderExecutor [ProtossNexus, ProtossRoboticsfacility, ProtossGateway, ProtossGateway] [] obs (HashMap.fromList [])
      else
        return $ BuildOrderExecutor orders' (queue' ++ affordableActions) obs abilities
    else do
      return $ BuildOrderExecutor buildOrder queue obs abilities

