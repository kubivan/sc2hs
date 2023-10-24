
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module TestBot(TestBot(..)) where

import Data.String
import Data.List (find)
import Data.Text (pack)

import Agent
import qualified Actions
import qualified Data.HashMap.Strict as HashMap
import Control.Monad
import Control.Monad.Writer.Strict(listen)


import qualified Proto.S2clientprotocol.Sc2api as A
import qualified Proto.S2clientprotocol.Sc2api_Fields as A
import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C

import Proto.S2clientprotocol.Raw as R
import Proto.S2clientprotocol.Raw_Fields as R
    ( alliance, pos, startLocations, unitType, units, abilityId, buildProgress )

import Lens.Micro ( (&), (.~), (&), (.~), (^.), (^..), to )

import Utils
import Proto.S2clientprotocol.Debug_Fields (unitTag, pos)

import AbilityId
import UnitTypeId
import Actions
import qualified GHC.Word
import UnitPool

import Grid
import Data.Sequence
import Data.Foldable (toList)
import Agent (UnitTraits, StaticInfo (unitTraits), Observation, AgentLog)
import Proto.S2clientprotocol.Sc2api_Fields (minerals, vespene)
import Conduit -- (filterC, mapC, runConduitPure)
import Grid (findPlacementPointInRadius, addMark, findPlacementPoint, printGrid, writeGridToFile)
import Footprint
import Utils (Pointable(make2D), fromTuple)
import UnitTypeId (UnitTypeId(ProtossProbe, ProtossPylon))
import AbilityId (AbilityId(HarvestGatherProbe))
import GHC.Real (fromIntegral)
import Footprint (getFootprint, Footprint (Footprint))
import UnitPool (unitsSelf)

import qualified Data.Vector as V
import Data.ByteString (putStr)
import Data.ByteString.Char8 (putStrLn)

type BuildOrder = [UnitTypeId]

toEnum' :: Enum e => GHC.Word.Word32 -> e
toEnum' = toEnum . fromIntegral

containsAbility :: UnitAbilities -> AbilityId -> Bool
containsAbility unitAbilities abilityId =
    any (abilityId `elem`) (HashMap.elems unitAbilities)

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

splitAffordable :: BuildOrder -> Cost -> (UnitTypeId -> Cost -> Bool ) -> (UnitTypeId -> Cost) -> (BuildOrder, BuildOrder)
splitAffordable bo reserved canAffordFn costFn = go empty bo reserved
  where
    go :: Seq UnitTypeId -> BuildOrder -> Cost -> (BuildOrder, BuildOrder)
    go affordable [] _ = (toList affordable, [])
    go affordable bo@(o:remaining) reserved
      | canAffordFn o reserved = go (affordable |> o) remaining (reserved + costFn o) `Utils.debug` ("can afford " ++ show o)
      | otherwise = (toList affordable, bo) `Utils.debug` ("cannot afford " ++ show o ++ "affordable " ++ show (toList affordable) ++ " remaining " ++ show bo)

data TestBot = Opening
  | BuildOrderExecutor Grid BuildOrder [Action] A.Observation

getExecutor :: Action -> GHC.Word.Word64
getExecutor (UnitCommand _ u _) = u
getExecutor (SelfCommand _ u) = u
getExecutor (PointCommand _ u _) = u

getCmd :: Action -> AbilityId
getCmd (UnitCommand a _ _) = a
getCmd (SelfCommand a _ ) = a
getCmd (PointCommand a _ _) = a

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

canAfford :: Observation -> StaticInfo -> UnitTypeId -> Bool
canAfford obs si id = canAfford' obs si id (Cost 0 0)

canAfford' :: Observation -> StaticInfo -> UnitTypeId -> Cost -> Bool
canAfford' obs si id r = Cost minerals vespene >= cost where
    minerals = fromIntegral $ obs ^. (#playerCommon . #minerals) --`Utils.debug` ("minerals: " ++ show minerals)
    vespene = fromIntegral $ obs ^. (#playerCommon . #vespene)
    cost = unitCost (unitTraits si) id + r

findBuilder :: Observation -> UnitTag
findBuilder obs = head $ runConduitPure $ yieldMany (unitsSelf obs)
  .| filterC (\a -> fromEnum ProtossProbe == fromIntegral (a ^. #unitType) )
  .| filterC (\x -> Prelude.null (x ^. #orders) || (HarvestGatherProbe` elem` map (\o -> toEnum' (o ^. #abilityId)) (x ^. #orders))) -- TODO: fix, add proper orders check
  .| mapC (\x -> x ^. #tag)
  .| sinkList

pylonRadius = 6.5

findPlacementPos :: Observation -> Grid -> UnitTypeId -> Maybe (Int, Int)
findPlacementPos obs grid ProtossPylon = findPlacementPoint grid (getFootprint ProtossPylon) (tilePos (nexus ^. #pos)) where
  nexus = findNexus obs

findPlacementPos obs grid id = go pylons where
    go :: [(Int, Int)] -> Maybe (Int, Int)
    go (p:ps) = case findPlacementPointInRadius grid (getFootprint id) p pylonRadius of
        Just res -> Just res
        Nothing -> go ps
    go [] = Nothing
    pylons = runConduitPure $ yieldMany (unitsSelf obs)
        .| filterC (\u -> fromEnum ProtossPylon == fromIntegral (u ^. #unitType) )
        .| filterC (\u -> u ^. #buildProgress == 1)
        .| mapC (\x -> tilePos $ x ^. #pos)
        .| sinkList
  
updateGrid :: Observation -> Grid -> Grid
updateGrid obs grid = foldl (\acc (fp, pos) -> addMark acc fp pos) grid (getFootprints <$> unitsSelf obs) where
  getFootprints :: Unit -> (Footprint, (Int, Int))
  getFootprints u = (getFootprint (toEnum' $ u ^. #unitType), tilePos $ u ^. #pos)

instance Agent TestBot where
    agentDebug (BuildOrderExecutor gr bo q _) = Prelude.putStrLn $ " buildOrder " ++ show bo ++ " queue: " ++ show q
    agentDebug _ = return ()

    agentRace _ = C.Protoss
    agentStep Opening si obs _ = command [SelfCommand AbilityId.TrainProbe nexus] >> return (BuildOrderExecutor grid' fourGate [] obs) where
        gi = gameInfo si
        grid' = updateGrid obs . gridFromImage $ gi ^. (#startRaw . #placementGrid)
        nexus = findNexus obs ^. #tag
        fourGate = [ ProtossPylon, ProtossGateway, ProtossAssimilator, ProtossAssimilator, ProtossCyberneticscore, ProtossGateway, ProtossPylon, ProtossPylon ]

    agentStep (BuildOrderExecutor grid buildOrder queue obsPrev) si obs abilities =
      command affordableActions >> Agent.debug [DebugText (pack ( "upos " ++ (show. tilePos $ c ^. #pos))) (c ^. #pos) | c <- unitsSelf obs ] >> return (BuildOrderExecutor grid' orders' (queue' ++ affordableActions) obs) where
        nexus = findNexus obs
        gi = gameInfo si
        grid' = updateGrid obs grid 
        (queue', interruptedAbilities) = processQueue queue ([], [])
        interruptedOrders = abilityToUnit (unitTraits si) . getCmd <$> interruptedAbilities
        processQueue :: [Action] -> ([Action], [Action]) -> ([Action], [Action])
        processQueue (a:as) (q', interrupted) = case findAssignee obs a of
            Nothing -> processQueue as (q', interrupted ++ [a])
            Just u -> if fromEnum (getCmd a) `elem` (u ^. #orders ^.. traverse . (A.abilityId . to fromIntegral)) then
                processQueue as (q' ++ [a], interrupted) else processQueue as (q', interrupted)
        processQueue [] res = res

        reservedResources = actionsCost si queue
        (affordableOrders, orders') = splitAffordable (interruptedOrders ++ buildOrder) reservedResources (canAfford' obs si) (unitCost (unitTraits si)) --`Utils.debug` (" reserved: " ++ show reservedResources ++ "buildOrder " ++ show buildOrder)

        affordableActions = createAction <$> affordableOrders

        createAction :: UnitTypeId -> Action
        createAction order
          | isBuildAbility ability = case findPlacementPos obs grid' order of
            Just tileP -> PointCommand ability (findBuilder obs) (fromTuple tileP) `Utils.debug` ("Found placement for " ++ show ability ++ " : " ++ show tileP)
            Nothing -> case findPlacementPoint grid' (getFootprint ProtossPylon) (tilePos (nexus ^. #pos)) of
              Just pp -> PointCommand BuildPylon (findBuilder obs) (fromTuple pp)
              Nothing -> error "cannot find pylon build pos"
          | otherwise = error "not implemented"
          where ability = unitToAbility (unitTraits si) order
