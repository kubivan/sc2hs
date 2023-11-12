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
import qualified Data.Conduit.List as CL
import Data.Foldable (toList)
import Data.HashMap.Strict qualified as HashMap
import Data.List (find, mapAccumL, sortBy, isPrefixOf, isSuffixOf)
import Data.Function (on)
import Data.Maybe qualified as Data
import Data.Sequence (Seq (..), empty, (|>))
import Data.String
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Vector qualified as V
import Footprint
import GHC.Real (fromIntegral)
import GHC.Word qualified
import Grid (Grid(..), addMark, findPlacementPoint, findPlacementPointInRadius, printGrid, gridToFile, gridToStr, gridFromImage)
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
import Units(mapTilePosC, closestC)
import Utils (distSquared)
import Proto.S2clientprotocol.Common (Point)
import UnitTypeId (UnitTypeId(NeutralVespenegeyser, NeutralRichvespenegeyser, NeutralProtossvespenegeyser, NeutralPurifiervespenegeyser, NeutralShakurasvespenegeyser))
import Data.Conduit.List (sourceList)

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

findPlacementPos :: Observation -> Grid -> Grid -> UnitTypeId -> Maybe (Int, Int)
findPlacementPos obs grid heightMap ProtossPylon = findPlacementPoint grid heightMap (getFootprint ProtossPylon) nexusPos (const True)
  where
    nexusPos = tilePos $ findNexus obs ^. #pos

findPlacementPos obs grid heightMap id = go pylons
  where
    go :: [(Int, Int)] -> Maybe (Int, Int)
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

addOrder :: UnitTag -> AbilityId -> Observation -> Observation
addOrder unitTag ability obs=
  obs & #rawData . A.units . traverse . filtered (\unit -> unit ^. #tag == unitTag) . #orders %~ (order :) --TODO: append order to the end?
  where
    order = defMessage & #abilityId .~ fromEnum' ability & #progress .~ -1 -- TODO: add target & progress

addUnit :: UnitTypeId -> Observation -> Observation
addUnit unitType obs =
  obs & #rawData . A.units %~ (unit unitType :)
  where
    unit :: UnitTypeId -> Units.Unit
    unit t = defMessage & #unitType .~ fromEnum' t & #buildProgress .~ -1 -- TODO: add target & progress

buildAction :: UnitTypeId -> Grid -> Cost -> MaybeStepMonad (Action, Cost, Grid)
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
  pos <- MaybeT . return $ findPlacementPos obs grid (heightMap si) order
  let grid' = addMark grid footprint pos
  let obs' = addOrder (builder ^. #tag) ability . addUnit order $ obs
  lift . agentPut $ (obs', grid) `Utils.dbg` (show order ++ " buildPos " ++ show pos ++ " builder " ++ show builder ++ " putting to the grid!!!!")

  let res = PointCommand ability (builder ^. #tag) (fromTuple pos)

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
      return (PointCommand BuildPylon (builder ^. #tag) (fromTuple pylonPos), cost, grid')

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
      nexuses = runC $ units .| unitTypeC ProtossNexus
      nexusCount = length nexuses
      optimalCount = assimCount * 3 + nexusCount * 16
  when (optimalCount - probeCount > 0) $ command [SelfCommand TrainProbe (n ^. #tag) | n <- nexuses]

reassignIdleProbes :: StepMonad ()
reassignIdleProbes = do
  obs <- agentObs
  let units = unitsSelf obs
      probes = units .| unitTypeC ProtossProbe
      mineralField = headMay $ runC $ probes
        .| filterC (\p -> HarvestGatherProbe `elem` map (\o -> toEnum' (o ^. #abilityId)) (p ^. #orders))
        .| mapC (\harvester -> head $ filter (\o -> HarvestGatherProbe == toEnum' (o ^. #abilityId)) (harvester ^. #orders))
        .| mapC (\harvestOrder -> harvestOrder ^. #targetUnitTag)

      closestMineral to = view #tag <$> runConduitPure (obsUnitsC obs .| unitTypeC NeutralMineralfield .| closestC to)

      idleWorkers = runC $ probes
        .| filterC (\u -> null$ u ^. #orders)

  command [UnitCommand HarvestGatherProbe utag (fromJust $ mineralField <|> closestMineral idle ) | idle <- idleWorkers, let utag = idle ^. #tag ]


clusterizeGrid:: Observation -> Grid -> Grid -> Point -> [TilePos]
clusterizeGrid obs grid heightMap start =
  let units = obsUnitsC obs
      geysers = runC $ units .| filterC (\u -> toEnum' (u ^. #unitType)  `elem` [NeutralVespenegeyser, NeutralRichvespenegeyser, NeutralProtossvespenegeyser, NeutralPurifiervespenegeyser, NeutralShakurasvespenegeyser])
      --groupedByHeight = geysers .| CL.groupBy ((==) `on` view (#pos . #z)) 
      sortedGeysers = sortBy (compare `on` distSquared start . to2D . view #pos) geysers

      farEnough :: Units.Unit -> Set.Set -> Bool
      farEnough x visited = case closestAnother of
        Just p -> distSquared (x ^. #pos) (p ^. #pos) > 15*15
        Nothing -> True
        where
          closestAnother = runConduitPure $ sourceList geysers .| filterC (`Set.notMember` visited) .| closestC x

      dropSecondGeysers :: [Units.Unit] -> [Units.Unit]
      dropSecondGeysers = go [] Set.empty
        where
          go acc visited (x:xs)
            | farEnough x acc visited = go (x:acc) (x `insert` visited) xs
            | otherwise = go acc (x `insert` visited) xs
          go acc _ [] = acc

      uniqueGeysers = dropSecondGeysers sortedGeysers

  in mapMaybe uniqueGeysers (findPlacementPointInRadius grid heightMap (getFootprint ProtossNexus) p pylonRadius * 2)

clusterizeGridSM :: Point -> StepMonad ()
clusterizeGridSM start = do
  (obs, grid) <- agentGet
  si <- agentAsk
  let res = clusterizeGrid obs grid (heightMap si) start
      unitT = toEnum' . view #unitType :: Units.Unit -> UnitTypeId
  debugTexts [("Expand: " ++ show x, x ^. #pos) | x <- res] `Utils.dbg` ("clusterize res: " ++ (show . length $ res))

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
    trainProbes
    si <- agentStatic
    (obs, grid0) <- agentGet
    let nexus = findNexus obs ^. #pos
    clusterizeGridSM nexus
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

