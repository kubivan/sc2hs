{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module TestBot (TestBot (..)) where

import AbilityId
-- (filterC, mapC, runConduitPure)

import AbilityId (AbilityId (HarvestGatherProbe))
import Actions
import Actions (UnitTag)
import Actions qualified
import Agent
import Agent (AgentLog, Observation, StaticInfo (unitTraits), UnitTraits)
import Conduit
import Control.Applicative (Alternative (..))
import Control.Monad
import Control.Monad.Writer.Strict (listen)
import Data.ByteString (putStr)
import Data.ByteString.Char8 (putStrLn)
import Data.Conduit.List (catMaybes)
import Data.Foldable (toList)
import Data.HashMap.Strict qualified as HashMap
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Maybe qualified as Data
import Data.Sequence (Seq (..), empty, (|>))
import Data.String
import Data.Text (pack)
import Data.Vector qualified as V
import Footprint
import Footprint (Footprint (Footprint), getFootprint)
import GHC.Real (fromIntegral)
import GHC.Word qualified
import Grid
import Grid (addMark, findPlacementPoint, findPlacementPointInRadius, printGrid, writeGridToFile)
import Lens.Micro (to, (&), (.~), (^.), (^..))
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
import Proto.S2clientprotocol.Sc2api_Fields (minerals, step, vespene)
import Proto.S2clientprotocol.Sc2api_Fields qualified as A
import UnitPool
import UnitPool (unitsSelf)
import UnitTypeId
import UnitTypeId (UnitTypeId (ProtossProbe, ProtossPylon))
import Utils
import Utils (Pointable (make2D), fromTuple)

type BuildOrder = [UnitTypeId]

toEnum' :: Enum e => GHC.Word.Word32 -> e
toEnum' = toEnum . fromIntegral

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
  | BuildOrderExecutor Grid BuildOrder [Action] A.Observation

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

canAfford' :: Observation -> StaticInfo -> UnitAbilities -> UnitTypeId -> Cost -> Bool
canAfford' obs si abilities id r = ability `elem` (abilities HashMap.! ProtossProbe) && (Cost minerals vespene >= cost)
  where
    minerals = fromIntegral $ obs ^. (#playerCommon . #minerals) -- `Utils.debug` ("minerals: " ++ show minerals)
    vespene = fromIntegral $ obs ^. (#playerCommon . #vespene)
    cost = unitCost (unitTraits si) id + r
    ability = unitToAbility (unitTraits si) id

findBuilder :: Observation -> [UnitTag] -> UnitTag
findBuilder obs taken =
  head $
    runConduitPure $
      yieldMany (unitsSelf obs)
        .| filterC (\a -> fromEnum ProtossProbe == fromIntegral (a ^. #unitType))
        .| filterC (\x -> Prelude.null (x ^. #orders) || (HarvestGatherProbe `elem` map (\o -> toEnum' (o ^. #abilityId)) (x ^. #orders))) -- TODO: fix, add proper orders check
        .| mapC (^. #tag)
        .| filterC (`notElem` taken)
        .| sinkList

pylonRadius = 6.5

findPlacementPos :: Observation -> Grid -> UnitTypeId -> Maybe (Int, Int)
findPlacementPos obs grid ProtossPylon = findPlacementPoint grid (getFootprint ProtossPylon) (tilePos (nexus ^. #pos))
  where
    nexus = findNexus obs
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

updateGrid :: Observation -> Grid -> Grid
updateGrid obs grid = foldl (\acc (fp, pos) -> addMark acc fp pos) grid (getFootprints <$> unitsSelf obs)
  where
    getFootprints :: Unit -> (Footprint, (Int, Int))
    getFootprints u = (getFootprint (toEnum' $ u ^. #unitType), tilePos $ u ^. #pos)

createAction :: StaticInfo -> Observation -> [UnitTag] -> Grid -> UnitTypeId -> Maybe Action
createAction si obs takenBuilders grid order = buildAction <|> pylonBuildAction
  where
    ability = unitToAbility (unitTraits si) order
    buildAction = guard (isBuildAbility ability) >> findPlacementPos obs grid order >>= \pos -> return $ PointCommand ability (findBuilder obs takenBuilders) (fromTuple pos)

    pylonBuildAction = do
      guard (isBuildAbility ability)
      let hasPylonsInProgress = not $ Prelude.null $ runConduitPure $ yieldMany (unitsSelf obs) .| filterC (\u -> fromEnum ProtossPylon == fromIntegral (u ^. #unitType)) .| filterC (\u -> u ^. #buildProgress < 1) .| sinkList

      if hasPylonsInProgress
        then Nothing
        else do
          let nexus = findNexus obs
          pylonPos <- findPlacementPoint grid (getFootprint ProtossPylon) (tilePos (nexus ^. #pos))
          return $ PointCommand BuildPylon (findBuilder obs takenBuilders) (fromTuple pylonPos)

instance Agent TestBot where
  agentDebug (BuildOrderExecutor gr bo q obs) = do
    let gameLoop = obs ^. #gameLoop
    writeGridToFile ("grids/grid" ++ show gameLoop ++ ".txt") gr
    Prelude.putStrLn $ show gameLoop ++ " buildOrder " ++ show bo ++ " queue: " ++ show q
  agentDebug _ = return ()

  agentRace _ = C.Protoss
  agentStep Opening si obs _ = command [SelfCommand AbilityId.TrainProbe nexus] >> return (BuildOrderExecutor grid' fourGate [] obs)
    where
      gi = gameInfo si
      grid' = updateGrid obs . gridFromImage $ gi ^. (#startRaw . #placementGrid)
      nexus = findNexus obs ^. #tag
      fourGate = [ProtossPylon, ProtossGateway, ProtossAssimilator, ProtossAssimilator, ProtossCyberneticscore, ProtossGateway, ProtossPylon, ProtossPylon]
  agentStep (BuildOrderExecutor grid buildOrder queue obsPrev) si obs abilities =
    command affordableActions >> Agent.debug [DebugText (pack ("upos " ++ (show . tilePos $ c ^. #pos))) (c ^. #pos) | c <- unitsSelf obs] >> return (BuildOrderExecutor grid' orders' (queue' ++ affordableActions) obs)
    where
      nexus = findNexus obs
      gi = gameInfo si
      grid' = updateGrid obs grid
      (queue', interruptedAbilities) = processQueue queue ([], [])
      interruptedOrders = abilityToUnit (unitTraits si) . getCmd <$> interruptedAbilities
      processQueue :: [Action] -> ([Action], [Action]) -> ([Action], [Action])
      processQueue (a : as) (q', interrupted) = case findAssignee obs a of
        Nothing -> processQueue as (q', interrupted ++ [a])
        Just u ->
          if fromEnum (getCmd a) `elem` (u ^. #orders ^.. traverse . (A.abilityId . to fromIntegral))
            then processQueue as (q' ++ [a], interrupted)
            else processQueue as (q', interrupted)
      processQueue [] res = res

      reservedResources = actionsCost si queue'

      splitAffordable :: BuildOrder -> Cost -> ([Action], BuildOrder)
      splitAffordable bo reserved = go Data.Sequence.empty bo reserved grid []
        where
          go :: Seq Action -> BuildOrder -> Cost -> Grid -> [UnitTag] -> ([Action], BuildOrder)
          go affordable [] _ _ _ = (toList affordable, [])
          go affordable bo@(o : remaining) reserved g builders = case tryCreateAction g builders of
            Just a -> go (affordable |> a) remaining (reserved + unitCost (unitTraits si) o) (addMark g (getFootprint o) (tilePos (getTarget a))) (getExecutor a : builders) -- `Utils.debug` ("can afford " ++ show o)
            Nothing -> (toList affordable, bo) `Utils.debug` ("cannot afford " ++ show o ++ "affordable " ++ show (toList affordable) ++ " remaining " ++ show bo)
            where
              tryCreateAction grid builders = guard (canAfford' obs si abilities o reserved) >> createAction si obs builders grid o

      (affordableActions, orders') = splitAffordable (interruptedOrders ++ buildOrder) reservedResources -- `Utils.debug` (" reserved: " ++ show reservedResources ++ "buildOrder " ++ show buildOrder)
