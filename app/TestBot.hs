{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module TestBot (TestBot (..)) where

import AbilityId

import Actions
import Actions (UnitTag)
import Actions qualified
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
import Grid (addMark, findPlacementPoint, findPlacementPointInRadius, printGrid, writeGridToFile, gridToString)
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
import UnitPool ( unitsSelf )
import UnitTypeId
import Utils
import Safe (headMay)
import Debug.Trace

type BuildOrder = [UnitTypeId]

data Cost = Cost {mineralCost :: Int, gasCost :: Int}
  deriving (Show, Eq, Ord)

instance Num Cost where
  a + b = Cost (mineralCost a + mineralCost b) (gasCost a + gasCost b)
  a - b = Cost (mineralCost a - mineralCost b) (gasCost a - gasCost b)
  a * b = Cost (mineralCost a * mineralCost b) (gasCost a * gasCost b)
  negate (Cost mc gc) = Cost (-mc) (-gc)
  abs (Cost mc gc) = Cost (abs mc) (abs gc)
  signum (Cost mc gc) = Cost (signum mc) (signum gc)
  fromInteger n = Cost (fromInteger n) (fromInteger n)

data TestBot
  = Opening
  | BuildOrderExecutor BuildOrder [Action] A.Observation

getExecutor :: Action -> GHC.Word.Word64
getExecutor (UnitCommand _ u _) = u
getExecutor (SelfCommand _ u) = u
getExecutor (PointCommand _ u _) = u

getCmd :: Action -> AbilityId
getCmd (UnitCommand a _ _) = a
getCmd (SelfCommand a _) = a
getCmd (PointCommand a _ _) = a

getTarget :: Action -> Point2D
getTarget (PointCommand _ _ t) = t

findAssignee :: Observation -> Action -> Maybe Unit
findAssignee obs a = find (\u -> u ^. #tag == getExecutor a) (UnitPool.unitsSelf obs)

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
gridUpdate obs grid = foldl (\acc (fp, pos) -> addMark acc fp pos `Utils.dbg` ("gridUpdate" ++ show fp ++ " " ++ show pos)) grid (getFootprints <$> unitsSelf obs)
  where
    getFootprints :: Unit -> (Footprint, (Int, Int))
    getFootprints u = (getFootprint (toEnum' $ u ^. #unitType), tilePos $ u ^. #pos)

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
  let ability = unitToAbility (unitTraits si) id
  return $ ability `elem` (abilities HashMap.! ProtossProbe)

findBuilder :: Observation -> Maybe UnitTag
findBuilder obs =
  headMay $
    runConduitPure $
      yieldMany (unitsSelf obs)
        .| filterC (\a -> fromEnum ProtossProbe == fromIntegral (a ^. #unitType))
        .| filterC (\x -> Prelude.null (x ^. #orders) || (HarvestGatherProbe `elem` map (\o -> toEnum' (o ^. #abilityId)) (x ^. #orders))) -- TODO: fix, add proper orders check
        .| mapC (^. #tag)
        .| sinkList

pylonRadius = 6.5

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
      runConduitPure $
        yieldMany (unitsSelf obs)
          .| filterC (\u -> fromEnum ProtossPylon == fromIntegral (u ^. #unitType))
          .| filterC (\u -> u ^. #buildProgress == 1)
          .| mapC (\x -> tilePos $ x ^. #pos)
          .| sinkList

addOrder :: Observation -> UnitTag -> AbilityId -> Observation
addOrder obs unitTag ability =
    obs & #rawData . A.units . traverse . filtered (\unit -> unit ^. #tag == unitTag) . #orders %~ (order :) --TODO: append order to the end?
    where
      order = defMessage & #abilityId .~ fromEnum' ability & #progress .~ -1 -- TODO: add target & progress

buildAction :: UnitTypeId -> Cost -> MaybeStepMonad (Action, Cost)
buildAction order reservedRes = do
  enabled <- lift . inBuildThechTree $ order
  guard enabled --`Utils.dbg` (show order ++ " enabled " ++ show enabled)
  (isAffordable, cost) <- lift $ canAfford order reservedRes
  guard isAffordable --`Utils.dbg` (show order ++ " affordable " ++ show isAffordable ++ " cost: " ++ show cost)

  si <- lift agentStatic
  (obs, grid) <- lift agentGet
  let ability = unitToAbility (unitTraits si) order
  let footprint = getFootprint order
  guard (isBuildAbility ability)
  pos <- MaybeT . return $ findPlacementPos obs grid order
  builder <- MaybeT . return $ findBuilder obs
  let grid' = addMark grid footprint pos
  let obs' = addOrder obs builder ability
  lift . agentPut $ (obs', grid') --`Utils.dbg` (show order ++ " buildPos " ++ show pos ++ " builder " ++ show builder ++ " putting to the grid!!!!") 

  let res = PointCommand ability builder (fromTuple pos)

  return (res, cost) --`Utils.dbg` ("buildAction PointCommand "  ++ show ability ++ " " ++ show pos ++ "\n" ++ gridToString grid')

pylonBuildAction :: Cost -> MaybeStepMonad (Action, Cost)
pylonBuildAction reservedRes = do
  (isAffordable, cost) <- lift $ canAfford ProtossPylon reservedRes
  guard isAffordable

  si <- lift agentStatic
  (obs, grid) <- lift agentGet
  let hasPylonsInProgress = not $ Prelude.null $ runConduitPure $ yieldMany (unitsSelf obs) .| filterC (\u -> fromEnum ProtossPylon == fromIntegral (u ^. #unitType)) .| filterC (\u -> u ^. #buildProgress < 1) .| sinkList
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
      return $ (PointCommand BuildPylon builder (fromTuple pylonPos), cost)

createAction :: Cost -> UnitTypeId -> MaybeStepMonad (Action, Cost)
createAction reserved order = buildAction order reserved -- <|> pylonBuildAction reserved

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
splitAffordable bo reserved = go bo Data.Sequence.empty reserved
  where
    go :: BuildOrder -> Seq Action -> Cost -> StepMonad ([Action], BuildOrder)
    go bo@(uid:remaining) acc reservedCost = do
      cres <- tryCreate reserved uid
      case cres of
        Just (action, cost) -> go remaining (acc |> action) (cost + reserved)
        Nothing -> return (toList acc, bo)
    go [] acc _ = return (toList acc, bo)

    tryCreate :: Cost -> UnitTypeId -> StepMonad (Maybe (Action, Cost))
    tryCreate reserved uid = runMaybeT $ createAction reserved uid

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
    return $ BuildOrderExecutor fourGate [] obs

  agentStep (BuildOrderExecutor buildOrder queue obsPrev) = do
    si <- agentStatic
    (obs, grid) <- agentGet
    let gameLoop = obs ^. #gameLoop
    let minerals = fromIntegral $ obs ^. (#playerCommon . #minerals) -- `Utils.debug` ("minerals: " ++ show minerals)
    let vespene = fromIntegral $ obs ^. (#playerCommon . #vespene)
    let gi = gameInfo si
    debug [DebugText (pack ("upos " ++ (show . tilePos $ c ^. #pos))) (c ^. #pos) | c <- unitsSelf obs]
    (queue', interruptedAbilities) <- processQueue queue ([], [])
    let reservedResources = actionsCost si queue'
    let interruptedOrders = abilityToUnit (unitTraits si) . getCmd <$> interruptedAbilities

    --(affordableActions, orders') <- splitAffordable (interruptedOrders ++ buildOrder) reservedResources `Utils.dbg` (show gameLoop ++ " resources " ++ show (minerals, vespene) ++  " buildOrder " ++ show (interruptedOrders ++ buildOrder))
    (affordableActions, orders') <- splitAffordable buildOrder reservedResources -- `Utils.dbg` (show gameLoop ++ " resources " ++ show (minerals, vespene) ++  " buildOrder " ++ show (buildOrder))
    command affordableActions
    return $ BuildOrderExecutor orders' (queue' ++ affordableActions) obs

