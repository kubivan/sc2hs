{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module TestBot (TestBot (..)) where

import AbilityId

import Actions
import Agent
import Conduit
import Control.Applicative (Alternative (..))
import Control.Monad
import Control.Monad.Writer.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.ByteString (putStr)
import Data.ProtoLens (defMessage)
import Data.Maybe ( isJust, catMaybes, fromJust, isNothing )
import Data.ByteString.Char8 (putStrLn)
import Data.Conduit.List (catMaybes)
import Data.Foldable (toList)
import Data.HashMap.Strict qualified as HashMap
import Data.List (find, mapAccumL)
import Data.Maybe qualified as Data
import Data.Sequence (Seq (..), empty, (|>))
import Data.String
import Data.Text (pack)
import Data.Vector qualified as V
import Footprint
import GHC.Real (fromIntegral)
import GHC.Word qualified
import Grid (Grid(..), addMark, findPlacementPoint, findPlacementPointInRadius, printGrid, writeGridToFile, gridToString, gridFromImage)
import Lens.Micro (to, (&), (.~), (^.), (^..), filtered, (%~))
import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import Proto.S2clientprotocol.Debug_Fields (pos, unitTag)
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

type BuildOrder = [UnitTypeId]

data TestBot
  = Opening
  | BuildOrderExecutor BuildOrder [Action] A.Observation UnitAbilities

findAssignee :: Observation -> Action -> Maybe Units.Unit
findAssignee obs a = find (\u -> u ^. #tag == getExecutor a) (obs ^. (#rawData . #units))

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

gridUpdate :: Observation -> Grid -> Grid
gridUpdate obs grid = foldl (\acc (fp, pos) -> addMark acc fp pos) grid (getFootprints <$> units) where -- `Utils.dbg` ("gridUpdate" ++ show fp ++ " " ++ show pos)) grid (getFootprints <$> units)
  --units = filter (\u -> toEnum' (u ^. #unitType) /= ProtossProbe) (obs ^. (#rawData . #units))
  units = obs ^. (#rawData . #units)
  getFootprints :: Units.Unit -> (Footprint, (Int, Int))
  getFootprints u = (getFootprint (toEnum' $ u ^. #unitType), tilePos $ u ^. #pos) -- `Utils.dbg` ("getFootPrint " ++ show (toEnum' (u ^. #unitType) :: UnitTypeId) ++ " " ++ show (tilePos $ u ^. #pos))

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
  let ability = unitToAbility (unitTraits si) id `Utils.dbg` ("abilities: " ++ show abilities)
  return $ ability `elem` (abilities HashMap.! ProtossProbe)

findBuilder :: Observation -> Maybe UnitTag
findBuilder obs =
  headMay $
    runC $
      unitsSelf obs
        .| unitTypeC ProtossProbe
        .| filterC (\x -> Prelude.null (x ^. #orders) || (length (x ^. #orders) == 1 && HarvestGatherProbe `elem` map (\o -> toEnum' (o ^. #abilityId)) (x ^. #orders))) -- TODO: fix, add proper orders check
        .| mapC (^. #tag)

pylonRadius = 6

findPlacementPos :: Observation -> Grid -> UnitTypeId -> Maybe (Int, Int)
findPlacementPos obs grid ProtossPylon = findPlacementPoint grid (getFootprint ProtossPylon) nexusPos
  where
    nexusPos = tilePos $ findNexus obs ^. #pos

findPlacementPos obs grid id = go pylons
  where
    go :: [(Int, Int)] -> Maybe (Int, Int)
    go (p : ps) = case findPlacementPointInRadius grid (getFootprint id) p pylonRadius of
      Just res -> Just res
      Nothing -> go ps
    go [] = Nothing
    pylons =
      runC $
        unitsSelf obs
          .| unitTypeC ProtossPylon
          .| filterC (\u -> u ^. #buildProgress == 1)
          .| mapC (\x -> tilePos $ x ^. #pos)

addOrder :: Observation -> UnitTag -> AbilityId -> Observation
addOrder obs unitTag ability =
    obs & #rawData . A.units . traverse . filtered (\unit -> unit ^. #tag == unitTag) . #orders %~ (order :) --TODO: append order to the end?
    where
      order = defMessage & #abilityId .~ fromEnum' ability & #progress .~ -1 -- TODO: add target & progress

buildAction :: UnitTypeId -> Grid -> Cost -> MaybeStepMonad (Action, Cost, Grid)
buildAction order grid reservedRes = do
  enabled <- lift . inBuildThechTree $ order
  guard enabled `Utils.dbg` (show order ++ " enabled " ++ show enabled)
  (isAffordable, cost) <- lift $ canAfford order reservedRes
  guard isAffordable

  si <- lift agentStatic
  (obs, _) <- lift agentGet
  let ability = unitToAbility (unitTraits si) order
  let footprint = getFootprint order
  guard (isBuildAbility ability)
  pos <- MaybeT . return $ findPlacementPos obs grid order
  builder <- MaybeT . return $ findBuilder obs
  let grid' = addMark grid footprint pos
  let obs' = addOrder obs builder ability
  lift . agentPut $ (obs', grid) `Utils.dbg` (show order ++ " buildPos " ++ show pos ++ " builder " ++ show builder ++ " putting to the grid!!!!")

  let res = PointCommand ability builder (fromTuple pos)

  return (res, cost, grid') `Utils.dbg` ("builder orders "  ++ show (getUnit obs' builder ^. #orders ) )

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
      let nexus = findNexus obs
      let footprint = getFootprint ProtossPylon
      pylonPos <- MaybeT . return $ findPlacementPoint grid footprint (tilePos (nexus ^. #pos))
      builder <- MaybeT . return $ findBuilder obs
      let grid' = addMark grid footprint pylonPos
      let obs' = addOrder obs builder AbilityId.BuildPylon
      lift . agentPut $ (obs', grid')
      return (PointCommand BuildPylon builder (fromTuple pylonPos), cost, grid')

createAction :: Grid -> Cost -> UnitTypeId -> MaybeStepMonad (Action, Cost, Grid)
createAction grid reserved order = do
  (isAffordable, cost) <- lift $ canAfford order reserved
  guard isAffordable `Utils.dbg` (show order ++ " affordable " ++ show isAffordable ++ " cost: " ++ show cost)

  buildAction order grid reserved <|> pylonBuildAction grid reserved

reassignIdleProbes :: StepMonad ()
reassignIdleProbes = do
  obs <- agentObs
  let units = unitsSelf obs
      probes = units .| unitTypeC ProtossProbe
      mineralField = head $ runC $ probes
        .| filterC (\p -> HarvestGatherProbe `elem` map (\o -> toEnum' (o ^. #abilityId)) (p ^. #orders))
        .| mapC (\harvester -> head$ filter (\o -> HarvestGatherProbe == toEnum' (o ^. #abilityId)) (harvester ^. #orders))
        .| mapC (\harvestOrder -> harvestOrder ^. #targetUnitTag)

      idleWorkers = runC $ probes
        .| filterC (\u -> null$ u ^. #orders)
        .| mapC (^. #tag)

  command [UnitCommand HarvestGatherProbe utag mineralField | utag <- idleWorkers ]

processQueue :: [Action] -> ([Action], [Action]) -> StepMonad ([Action], [Action])
processQueue (a : as) (q', interrupted) = do
  si <- agentStatic
  (obs, grid) <- agentGet
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

instance Agent TestBot where
  agentDebug _ = return ()

  agentRace _ = C.Protoss
  agentStep Opening = do
    si <- agentStatic
    (obs, grid) <- agentGet
    let gi = gameInfo si
    agentPut (obs, gridUpdate obs grid)
    let nexus = findNexus obs ^. #tag
    let fourGate = [ProtossPylon, ProtossGateway, ProtossCyberneticscore, ProtossGateway]

    command [SelfCommand AbilityId.TrainProbe nexus]
    return $ BuildOrderExecutor (ProtossForge:replicate 50 ProtossPhotoncannon) [] obs (HashMap.fromList [])

  agentStep (BuildOrderExecutor buildOrder queue obsPrev abilitiesPrev) = do
    debugUnitPos
    reassignIdleProbes
    si <- agentStatic
    (obs, grid0) <- agentGet
    let nexus = findNexus obs ^. #pos
    let gameLoop = obs ^. #gameLoop
    abilities <- agentAbilities
    if unitsChanged obs obsPrev || abilities /= abilitiesPrev then do
      --command [Chat "unitsChanged !!!: "]
      agentPut (obs, gridUpdate obs (gridFromImage $ gameInfo si ^. (#startRaw . #placementGrid))) -- >> command [Chat $ pack "grid updated"]
      (queue', interruptedAbilities) <- processQueue queue ([], [])
      let reservedResources = actionsCost si queue'
      let interruptedOrders = abilityToUnit (unitTraits si) . getCmd <$> interruptedAbilities
      unless (null interruptedOrders) $
        command [Chat $ pack ("interrupted: " ++ show interruptedOrders)]

      (affordableActions, orders') <- splitAffordable (interruptedOrders ++ buildOrder) reservedResources -- `Utils.dbg` (show gameLoop ++ " resources " ++ show (minerals, vespene) ++  " buildOrder " ++ show (buildOrder))

      unless (null affordableActions) $ do
        command [Chat $ pack ("scheduling: " ++ show affordableActions ++ " buildOrder: " ++ show (take 5 orders'))]

      debugTexts [("planned " ++ show (getCmd a), defMessage & #x .~ (getTarget a) ^. #x & #y .~ (getTarget a) ^. #y & #z .~ 10) | a <- affordableActions]
      command affordableActions
      if null orders' then -- restart build order
        return $ BuildOrderExecutor (replicate 50 ProtossPhotoncannon) [] obs (HashMap.fromList [])
      else
        return $ BuildOrderExecutor orders' (queue' ++ affordableActions) obs abilities
    else do
      return $ BuildOrderExecutor buildOrder queue obs abilities

