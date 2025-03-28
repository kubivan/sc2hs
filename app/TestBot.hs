{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TestBot (TestBot (..)) where

import AbilityId
import Actions
import Agent (
    Agent (..),
    AgentDynamicState (..),
    MaybeStepMonad,
    StaticInfo (expandsPos, gameInfo, heightMap, unitTraits),
    StepMonad,
    StepPlan,
    UnitAbilities,
    agentAbilities,
    agentAsk,
    agentChat,
    agentGet,
    agentObs,
    agentPut,
    agentStatic,
    command,
    debugTexts,
 )
import Conduit (filterC, findC, headC, lengthC, mapC, runConduitPure, (.|))
import Control.Applicative (Alternative (..))
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Writer.Strict
import Data.Conduit.List qualified as CL
import Data.Foldable (toList)
import Data.Foldable qualified as Seq
import Data.Function (on)
import Data.HashMap.Strict qualified as HashMap
import Data.List (minimumBy, partition)
import Data.Maybe (fromJust, isJust)
import Data.Ord (comparing)
import Data.ProtoLens (defMessage)
import Data.Sequence (Seq (..), empty, (|>))
import Data.Set qualified as Set
import Footprint (getFootprint)
import Grid (
    Grid (..),
    addMark,
    findPlacementPoint,
    gridFromImage,
    gridMerge,
    gridPixelSafe,
    gridPlace,
    pixelIsRamp,
 )
import Lens.Micro (to, (&), (.~), (^.), (^..))
import Lens.Micro.Extras (view)
import Observation (
    Cost (Cost),
    Observation,
    addOrder,
    addUnit,
    buildingsSelfChanged,
    findNexus,
    getUnit,
    gridUpdate,
    obsUnitsC,
    unitsSelf,
 )
import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Raw qualified as R

import Proto.S2clientprotocol.Sc2api_Fields qualified as A
import Safe (headMay, minimumByMay)
import System.Random (Random, StdGen, newStdGen, randomR)
import UnitTypeId
import Units (
    Unit,
    closestC,
    isAssimilator,
    isBuilding,
    isBuildingType,
    isMineral,
    mapTilePosC,
    runC,
    toEnum',
    unitIdleC,
    unitTypeC,
 )
import Utils (
    Pointable,
    TilePos,
    dbg,
    distManhattan,
    distSquared,
    fromTuple,
    tilePos,
    toPoint2D,
    triPartition,
 )

import AgentBulidUtils

type BuildOrder = [UnitTypeId]

data TestDynamicState = TestDynamicState
    { observation :: Observation
    , grid :: Grid
    , randGen :: StdGen
    , dsArmy :: Army
    }

newtype ArmySquad = ArmySquad ([UnitTag], ArmyUnitData)

data Army = Army
    { armyUnits :: HashMap.HashMap UnitTag ArmyUnitData
    , armyUnitsPos :: Set.Set TilePos
    }

emptyArmy :: Army
emptyArmy = Army HashMap.empty Set.empty

-- Update the AgentDynamicState instance for TestDynamicState
instance AgentDynamicState TestDynamicState where
    getObs (TestDynamicState obs _ _ _) = obs
    getGrid (TestDynamicState _ grid _ _) = grid

    setObs obs (TestDynamicState _ grid gen army) = TestDynamicState obs grid gen army
    setGrid grid (TestDynamicState obs _ gen army) = TestDynamicState obs grid gen army
    dsUpdate obs grid (TestDynamicState _ _ gen army) = TestDynamicState obs grid gen army

setRandGen :: StdGen -> TestDynamicState -> TestDynamicState
setRandGen gen (TestDynamicState obs grid _ army) = TestDynamicState obs grid gen army

-- Adding a function to retrieve random values from the dynamic state
getRandValue :: (Random a) => (a, a) -> TestDynamicState -> (a, TestDynamicState)
getRandValue range (TestDynamicState obs grid gen army) =
    let (value, newGen) = randomR range gen
     in (value, TestDynamicState obs grid newGen army)

data ArmyUnitData = ArmyUnitData
    { auVisitedTiles :: Set.Set TilePos
    , auUnvisitedEdge :: Set.Set TilePos
    }

updateArmyData :: UnitTag -> ArmyUnitData -> TestDynamicState -> TestDynamicState
updateArmyData tag newUnitData ds =
    let army = dsArmy ds
        updatedArmy = army{armyUnits = HashMap.insert tag newUnitData (armyUnits army)}
     in ds{dsArmy = updatedArmy}

agentUpdateArmyPositions :: StepMonad TestDynamicState ()
agentUpdateArmyPositions = do
    ds <- agentGet
    obs <- agentObs -- allUnitPos = Set.fromList $ runC $ obsUnitsC obs .| filterC (not.isBuilding) .| mapC (view #pos) .| mapC tilePos
    let armyPoss = runC $ unitsSelf obs .| filterC isArmyUnit .| mapC (tilePos . view #pos)
        army' = (dsArmy ds){armyUnitsPos = Set.fromList armyPoss}

    agentPut $ ds{dsArmy = army'}

-- Update the visited tiles for a unit in the army
updateVisitedTile :: UnitTag -> TilePos -> StepMonad TestDynamicState ()
updateVisitedTile tag tile = do
    ds <- agentGet
    let grid = getGrid ds
        army = armyUnits . dsArmy $ ds
        defaultUnitData = ArmyUnitData Set.empty (Set.fromList . Seq.toList $ neighbors tile grid)
        unitData = HashMap.lookupDefault defaultUnitData tag army
        newVisited = Set.insert tile (auVisitedTiles unitData)
        newUnvisitedEdge =
            Set.foldl'
                ( \accEdge vt ->
                    let unvisitedNs = Set.fromList [n | n <- Seq.toList (neighbors vt grid), n `Set.notMember` newVisited]
                     in accEdge `Set.union` unvisitedNs
                )
                Set.empty
                (auUnvisitedEdge unitData)

        newUnitData = unitData{auVisitedTiles = newVisited, auUnvisitedEdge = newUnvisitedEdge}

    agentPut $ updateArmyData tag newUnitData ds -- ds { dsArmy = {Army (HashMap.insert tag newUnitData army)} }

-- Check if a tile has been visited
-- hasVisitedTile :: UnitTag -> TilePos -> TestArmy -> Bool
-- hasVisitedTile tag tile army =
--   case HashMap.lookup tag (armyUnits army) of
--     Just unitData -> Set.member tile (visitedTiles unitData)
--     Nothing       -> False

data TestBot
    = Opening
    | BuildOrderExecutor BuildOrder [Action] Observation UnitAbilities
    | BuildArmyAndWin Observation

inBuildThechTree :: UnitTypeId -> StepMonad d Bool
inBuildThechTree id = do
    abilities <- agentAbilities
    si <- agentStatic
    let ability = unitToAbility (unitTraits si) id -- `Utils.dbg` ("abilities: " ++ show abilities)
    return $ ability `elem` (abilities HashMap.! ProtossProbe)

buildAction :: (AgentDynamicState d) => UnitTypeId -> Grid -> Cost -> MaybeStepMonad d (Action, Cost, Grid)
buildAction ProtossAssimilator grid reservedRes = do
    (isAffordable, cost) <- lift $ canAfford ProtossAssimilator reservedRes
    guard isAffordable
    obs <- lift agentObs
    builder <- MaybeT . return $ findBuilder obs
    geyser <- MaybeT . return $ findFreeGeyser obs

    let res = UnitCommand BuildAssimilator builder geyser

    return (res, cost, grid) -- `Utils.dbg` ("building Assimilator target " ++ show geyser ++ " builder " ++ show builder ++ " putting to the grid!!!!")
buildAction order grid reservedRes = do
    enabled <- lift . inBuildThechTree $ order
    guard enabled -- `Utils.dbg` (show order ++ " enabled " ++ show enabled)
    (isAffordable, cost) <- lift $ canAfford order reservedRes
    guard isAffordable

    si <- lift agentStatic
    ds <- lift agentGet
    let ability = unitToAbility (unitTraits si) order
        footprint = getFootprint order
        obs = getObs ds
        grid = getGrid ds
    guard (isBuildAbility ability)
    builder <- MaybeT . return $ findBuilder obs
    pos <- MaybeT . return $ findPlacementPos obs (expandsPos si) grid (heightMap si) order
    let grid' = addMark grid footprint pos
        obs' = addOrder (builder ^. #tag) ability . addUnit order $ obs
    lift . agentPut $
        dsUpdate obs' grid' ds
            `Utils.dbg` (show order ++ " buildPos " ++ show pos ++ " builder " ++ show builder ++ " putting to the grid!!!!")

    let res = PointCommand ability builder (fromTuple pos)

    return (res, cost, grid') `Utils.dbg` ("builder orders " ++ show (builder ^. #orders))

distantEnough :: (Foldable t, Pointable p1, Pointable p2) => t p2 -> Float -> p1 -> Bool
distantEnough units radius pos = all (\p -> distSquared pos p >= radius * radius) units

pylonBuildAction :: (AgentDynamicState d) => Grid -> Cost -> MaybeStepMonad d (Action, Cost, Grid)
pylonBuildAction grid reservedRes = do
    (isAffordable, cost) <- lift $ canAfford ProtossPylon reservedRes
    guard isAffordable

    si <- lift agentStatic
    ds <- lift agentGet
    let obs = getObs ds
        hasPylonsInProgress = not $ Prelude.null $ runC $ unitsSelf obs .| unitTypeC ProtossPylon .| filterC (\u -> u ^. #buildProgress < 1)
    guard (not hasPylonsInProgress) -- `Utils.dbg` ("hasPylonsInProgress: " ++ show hasPylonsInProgress)
    builder <- MaybeT . return $ findBuilder obs
    let nexus = findNexus obs `Utils.dbg` ("builder: " ++ show builder)
        footprint = getFootprint ProtossPylon
        findPylonPlacementPoint = findPlacementPoint grid (heightMap si) footprint (tilePos (builder ^. #pos))
        pylonsPos = runC $ unitsSelf obs .| unitTypeC ProtossPylon .| mapTilePosC
        pylonCriteria = distantEnough pylonsPos
    pylonPos <- MaybeT . return $ findPylonPlacementPoint (pylonCriteria pylonRadius)
    -- <|> findPylonPlacementPoint (pylonCriteria (pylonRadius / 2))
    -- <|> findPylonPlacementPoint (const True)

    let grid' = addMark grid footprint pylonPos
    let obs' = addOrder (builder ^. #tag) AbilityId.BuildPylon obs
    lift . agentPut $ dsUpdate obs' grid' ds
    return (PointCommand BuildPylon builder (fromTuple pylonPos), cost, grid') `Utils.dbg` ("pylonPos: " ++ show pylonPos)

createAction :: (AgentDynamicState d) => Grid -> Cost -> UnitTypeId -> MaybeStepMonad d (Action, Cost, Grid)
createAction grid reserved order = do
    (isAffordable, cost) <- lift $ canAfford order reserved
    guard isAffordable -- `Utils.dbg` (show order ++ " affordable " ++ show isAffordable ++ " cost: " ++ show cost)
    buildAction order grid reserved <|> pylonBuildAction grid reserved

trainProbes :: (AgentDynamicState d) => StepMonad d ()
trainProbes = do
    obs <- agentObs
    let units = unitsSelf obs
        probeCount = runConduitPure $ units .| unitTypeC ProtossProbe .| lengthC
        assimCount = runConduitPure $ units .| unitTypeC ProtossAssimilator .| lengthC
        nexuses :: [Units.Unit]
        nexuses = runC $ units .| unitTypeC ProtossNexus .| filterC (\n -> (n ^. #buildProgress) == 1) .| unitIdleC
        nexusCount = length nexuses
        optimalCount = assimCount * 3 + nexusCount * 16
    when (optimalCount - probeCount > 0) $ command [SelfCommand TrainProbe n | n <- nexuses]

-- `Utils.dbg` ("trainProbes: optCount" ++ show optimalCount ++ " probes: " ++ show probeCount )

-- TODO: merge unitHasOrder && unitIsHarvesting
unitHasOrder :: AbilityId -> Units.Unit -> Bool
unitHasOrder order u = order `elem` orders
  where
    orders = toEnum' . view #abilityId <$> u ^. #orders

reassignIdleProbes :: (AgentDynamicState d) => StepMonad d ()
reassignIdleProbes = do
    obs <- agentObs
    let units = unitsSelf obs
        probes = units .| unitTypeC ProtossProbe

        mineralField =
            runConduitPure $
                probes
                    .| filterC (\p -> HarvestGatherProbe `elem` map (\o -> toEnum' (o ^. #abilityId)) (p ^. #orders))
                    .| mapC (\harvester -> head $ filter (\o -> HarvestGatherProbe == toEnum' (o ^. #abilityId)) (harvester ^. #orders))
                    .| mapC (\harvestOrder -> harvestOrder ^. #targetUnitTag)
                    .| mapC (getUnit obs)
                    .| filterC isJust
                    .| mapC fromJust
                    .| headC

        harvesters =
            probes
                .| filterC unitIsHarvesting

        nexuses =
            runC $
                unitsSelf obs
                    .| unitTypeC ProtossNexus
                    .| filterC ((== 1) . view #buildProgress)

        assimilators =
            runC $
                unitsSelf obs
                    .| filterC isAssimilator
                    .| filterC ((== 1) . view #buildProgress)

        (vespeneHarversters, mineralHarvesters) = partition (unitIsVespeneHarvester assimilators) (runC harvesters)

        (nexusesUnder, nexusesIdeal, nexusesOver) = triPartition (\n -> (n ^. #assignedHarvesters) `compare` (n ^. #idealHarvesters)) nexuses
        (assimUnder, assimIdeal, assimOver) = triPartition (\n -> (n ^. #assignedHarvesters) `compare` (n ^. #idealHarvesters)) assimilators

        idleWorkers = runC $ probes .| unitIdleC

    -- trace ("nexuses: " ++ show (length nexusesUnder, length nexusesIdeal, length nexusesOver)) (return ())
    if not . null $ assimUnder
        then do
            -- trace "staffing assimilators: taking idle + nexuses" (return ())
            let probePool = idleWorkers ++ workersFromOverAssims ++ workersAroundIdealAndOverStaffedNexuses
                workersFromOverAssims = getOverWorkersFrom assimOver vespeneHarversters
                workersAroundIdealAndOverStaffedNexuses =
                    runC $
                        CL.sourceList mineralHarvesters
                            .| filterC (unitIsAssignedToAny (nexusesOver ++ nexusesIdeal)) -- TODO: use manhattan
            command
                [ UnitCommand HarvestGatherProbe harvester assimilator
                | (assimilator, harvester) <- zip assimUnder probePool
                ]
        else
            if not . null $ nexusesUnder
                then do
                    -- assimilators are staffed, deal with nexuses
                    -- trace "staffing nexusesUnder" (return ())
                    let probePool = idleWorkers ++ workersFromOverAssims ++ workersAroundOverNexuses
                        workersFromOverAssims = getOverWorkersFrom assimOver vespeneHarversters
                        workersAroundOverNexuses = getOverWorkersFrom nexusesOver mineralHarvesters
                        closestMineralTo :: Units.Unit -> Units.Unit
                        closestMineralTo nexus =
                            fromJust $
                                runConduitPure $
                                    obsUnitsC obs .| filterC isMineral .| findC (\m -> distManhattan (m ^. #pos) (nexus ^. #pos) <= 12)
                    command
                        [ UnitCommand HarvestGatherProbe harvester (closestMineralTo nexus)
                        | (nexus, harvester) <- zip nexusesUnder probePool
                        ]
                else do
                    -- trace "staffing: just idle probes" (return ())
                    let closestMineral to = runConduitPure $ obsUnitsC obs .| filterC isMineral .| closestC to
                    -- TODO: obsolete, rewrite
                    command [UnitCommand HarvestGatherProbe idle (fromJust $ mineralField <|> closestMineral idle) | idle <- idleWorkers]

buildPylons :: (AgentDynamicState d) => MaybeStepMonad d ()
buildPylons = do
    ds <- lift agentGet

    let obs = getObs ds
        grid = getGrid ds
        foodCap = fromIntegral $ obs ^. (#playerCommon . #foodCap) -- `Utils.debug` ("minerals: " ++ show minerals)
        foodUsed = fromIntegral $ obs ^. (#playerCommon . #foodUsed)

        incompletePylonsCount =
            length $
                runC $
                    unitsSelf obs
                        .| unitTypeC ProtossPylon
                        .| filterC (\x -> x ^. #buildProgress < 1)
        orderedPylonsCount =
            length $
                runC $
                    unitsSelf obs
                        .| unitTypeC ProtossProbe
                        .| filterC (unitHasOrder BuildPylon)

        expectedFoodCap = 8 * incompletePylonsCount + 8 * orderedPylonsCount

    -- TODO:
    guard (foodCap + expectedFoodCap - foodUsed < 2)
    (act, _, _) <- pylonBuildAction grid (Cost 0 0)
    lift $ command [act]

processQueue :: (AgentDynamicState d) => [Action] -> ([Action], [Action]) -> StepMonad d ([Action], [Action])
processQueue (a : as) (q', interrupted) = do
    obs <- agentObs
    case findAssignee obs a of
        Nothing -> processQueue as (q', interrupted ++ [a])
        Just u ->
            if fromEnum (getCmd a) `elem` (u ^. #orders ^.. traverse . (A.abilityId . to fromIntegral))
                then processQueue as (q' ++ [a], interrupted)
                else processQueue as (q', interrupted)
processQueue [] res = return res

splitAffordable :: (AgentDynamicState d) => BuildOrder -> Cost -> StepMonad d ([Action], BuildOrder)
splitAffordable bo reserved = agentGet >>= (\ds -> go bo Data.Sequence.empty (getGrid ds) reserved) -- `Utils.dbg` ("splitAffordable " ++ show bo ++ " reserved" ++ show reserved)
  where
    go :: (AgentDynamicState d) => BuildOrder -> Seq Action -> Grid -> Cost -> StepMonad d ([Action], BuildOrder)
    go bo@(uid : remaining) acc grid reservedCost = do
        (si, _) <- agentAsk
        cres <- tryCreate grid reservedCost uid
        case cres of
            Just (action, cost, grid') -> go remainingOrBo (acc |> action) grid' (cost + reserved)
              where
                -- if we fallback to the BuildPylon, don't remove the order from BO
                remainingOrBo = if getCmd action == unitToAbility (unitTraits si) uid then remaining else bo
            Nothing -> return (toList acc, bo)
    go [] acc _ _ = return (toList acc, [])

    tryCreate :: (AgentDynamicState d) => Grid -> Cost -> UnitTypeId -> StepMonad d (Maybe (Action, Cost, Grid))
    tryCreate grid reserved uid = runMaybeT $ createAction grid reserved uid

debugUnitPos :: WriterT StepPlan (StateT TestDynamicState (Reader (StaticInfo, UnitAbilities))) ()
debugUnitPos = agentObs >>= \obs -> debugTexts [("upos " ++ show (tilePos . view #pos $ c), c ^. #pos) | c <- runC $ unitsSelf obs]

agentResetGrid :: (AgentDynamicState d) => StepMonad d ()
agentResetGrid = do
    obs <- agentObs
    ds <- agentGet
    gridPlacementStart <- gridFromImage . view (#startRaw . #placementGrid) . gameInfo <$> agentStatic
    gridPathingStart <- gridFromImage . view (#startRaw . #pathingGrid) . gameInfo <$> agentStatic

    let gridMerged = gridMerge pixelIsRamp gridPlacementStart gridPathingStart
        grid' = gridUpdate obs gridMerged

    agentPut $ setGrid grid' ds

agentUpdateGrid f = do
    ds <- agentGet
    agentPut $ setGrid (f (getGrid ds)) ds

randomArmyFiddling :: StepMonad TestDynamicState ()
randomArmyFiddling = do
    obs <- agentObs
    ds <- agentGet
    -- Retrieve army units and enemies
    let armyUs = runC $ unitsSelf obs .| filterC isArmyUnit
        grid = getGrid ds -- Retrieve the grid from the dynamic state

    -- Execute a random command for each unit in the army
    mapM_
        ( \u ->
            let unitData = HashMap.lookupDefault (ArmyUnitData Set.empty Set.empty) (u ^. #tag) (armyUnits . dsArmy $ ds)
             in randCmd2 grid unitData u
        )
        armyUs

randCmd2 :: Grid -> ArmyUnitData -> Unit -> StepMonad TestDynamicState ()
randCmd2 grid udata u = do
    obs <- agentObs
    let enemies = runC $ obsUnitsC obs .| filterC isEnemy
    -- Check if there's an enemy in range
    case enemyInRange u enemies of
        Just e -> command [Actions.UnitCommand Attack u e] -- Attack the enemy
        Nothing ->
            if not . null . view #orders $ u
                then return ()
                else do
                    -- Move to a random neighboring position if no enemy is in range
                    si <- agentStatic
                    ds <- agentGet
                    let army = dsArmy ds
                        allUnitPos = armyUnitsPos army
                        unvisitedNeighbors = filter (`Set.notMember` allUnitPos) $ filter (\p -> p `Set.notMember` auVisitedTiles udata) (neighbors upos grid)

                    pos <- calcMovePos unvisitedNeighbors

                    command [PointCommand Move u (toPoint2D pos)] -- Move to the random position
  where
    upos = tilePos (u ^. #pos)
    nearest :: (Pointable p) => p -> [p] -> p
    nearest p = minimumBy (compare `on` distSquared p)

    calcMovePos [] = return $ nearest upos (Set.toList (auUnvisitedEdge udata))
    calcMovePos candidates = do
        ds <- agentGet
        rnd <- randGen <$> agentGet -- Retrieve the current random generator
        let scored = scoreMoveCandidates upos udata candidates
            (wrandPos, rnd') = weightedRandomChoice scored rnd -- `Utils.dbg` ("scored: " ++ show scored)
        agentPut $ setRandGen rnd' ds -- Update the random generator in the dynamic state
        updateVisitedTile (u ^. #tag) wrandPos
        return wrandPos

scoreMoveCandidates :: TilePos -> ArmyUnitData -> [TilePos] -> [(TilePos, Double)]
scoreMoveCandidates upos udata = map (\tile -> (tile, calcScore tile))
  where
    calcScore tile = baseScore + unvisitedScore tile -- + closeToUnvisitedEdgeScore tile -- + enemyScore tile
    baseScore = 0.0
    unvisitedScore tile
        | tile `Set.member` auVisitedTiles udata = 1.0
        | otherwise = 10.0

    closeToUnvisitedEdgeScore tile =
        Set.foldl'
            ( \score edgeTile ->
                let distToEdge :: TilePos -> Float
                    distToEdge = distSquared (edgeTile :: TilePos)
                    isCloserToEdgeThen :: TilePos -> TilePos -> Bool
                    a `isCloserToEdgeThen` b = distToEdge a < distToEdge b
                 in if tile `isCloserToEdgeThen` upos then score + 5.0 else score
            )
            0
            (auUnvisitedEdge udata)

weightedRandomChoice :: [(a, Double)] -> StdGen -> (a, StdGen)
weightedRandomChoice weightedItems gen = (selectItem weightedItems r, newGen)
  where
    totalWeight = sum (map snd weightedItems) -- Sum of all weights
    (r, newGen) = randomR (0, totalWeight) gen -- Generate a random number between 0 and the total weight

selectItem :: [(a, Double)] -> Double -> a
selectItem ((item, weight) : xs) r
    | r <= weight = item -- If random value is within current item's weight, return it
    | otherwise = selectItem xs (r - weight) -- Otherwise, subtract the weight and move to the next item
selectItem [] _ = error "weightedRandomChoice: empty list"

-- Get the direction vector from one position to another
directionTo :: Point2D -> Point2D -> (Int, Int)
directionTo p1 p2 = (round $ signum (p2 ^. #x - p1 ^. #x), round $ signum (p2 ^. #y - p1 ^. #y))

-- Get the nearest enemy unit
nearestEnemy :: Unit -> [Unit] -> Maybe Unit
nearestEnemy u = minimumByMay (comparing (distSquared (u ^. #pos) . (^. #pos)))

nearestTarget :: Unit -> [Point2D] -> Maybe Point2D
nearestTarget u = minimumByMay (comparing (distSquared (toPoint2D $ u ^. #pos)))

-- Function to select a random element from a list of weights
weightedRandom :: [Double] -> StdGen -> (Int, StdGen)
weightedRandom weights gen =
    let totalWeight = sum weights
        (randValue, newGen) = randomR (0, totalWeight) gen -- Random value between 0 and the total weight
     in (selectIndex weights randValue, newGen)

-- Function to select the index based on the random value and the cumulative sum of weights
selectIndex :: [Double] -> Double -> Int
selectIndex weights randValue = go weights randValue 0
  where
    go (w : ws) rv idx
        | rv <= w = idx -- If random value is less than or equal to current weight, return the index
        | otherwise = go ws (rv - w) (idx + 1) -- Subtract the weight and continue
    go [] _ idx = idx -- Default case, in case something goes wrong (should not happen with proper weights)

-- Get neighboring tiles
neighbors :: TilePos -> Grid -> [TilePos]
neighbors p@(x, y) grid =
    [ (x + dx, y + dy)
    | dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]
    , dx /= 0 || dy /= 0 -- Exclude points on the same vertical line
    , let pixel = gridPixelSafe grid (x + dx, y + dy)
    , pixel /= Just '#'
    ]

-- Check if an enemy is in range
enemyInRange :: Unit -> [Unit] -> Maybe Unit
enemyInRange u enemies =
    headMay $ filter (\e -> distSquared (e ^. #pos) (u ^. #pos) <= 6 * 6) enemies -- Stalker attack range of 6

-- Define if a unit is considered an army unit
isArmyUnit :: Unit -> Bool -- TODO: remove protoss specific consts
isArmyUnit u = ProtossProbe /= utype && (not . isBuildingType $ utype)
  where
    utype = toEnum' (u ^. #unitType)

-- Define an enemy unit filter
isEnemy :: Unit -> Bool
isEnemy u = (u ^. #alliance) == R.Enemy -- Enemy alliance code

agentUpdateArmy :: Observation -> StepMonad TestDynamicState ()
agentUpdateArmy obsPrev = agentUpdateArmyPositions -- TODO: no diff with obsPrev

selfBuildingsCount :: Observation -> Int
selfBuildingsCount obs = length . runC $ unitsSelf obs .| filterC isBuilding

instance Agent TestBot TestDynamicState where
    type DynamicState TestBot = TestDynamicState
    makeDynamicState _ obs grid = do
        gen <- newStdGen -- Initialize a new random generator
        return $ TestDynamicState obs grid gen emptyArmy
    agentRace _ = C.Protoss
    agentStep Opening = do
        si <- agentStatic
        ds <- agentGet

        let gi = gameInfo si
            obs = getObs ds
            grid = getGrid ds
        agentPut $ setGrid (gridUpdate obs grid) ds
        let nexus = findNexus obs
            fourGateBuild = [ProtossPylon, ProtossAssimilator, ProtossGateway, ProtossCyberneticscore, ProtossAssimilator, ProtossGateway]
            expandBuild = [ProtossNexus, ProtossRoboticsfacility, ProtossGateway, ProtossGateway]

        command [SelfCommand AbilityId.TrainProbe nexus]
        return $ BuildOrderExecutor (fourGateBuild ++ expandBuild) [] obs (HashMap.fromList [])
    agentStep (BuildOrderExecutor buildOrder queue obsPrev abilitiesPrev) = do
        debugUnitPos
        reassignIdleProbes
        trainProbes
        si <- agentStatic
        ds <- agentGet
        let obs = getObs ds
        abilities <- agentAbilities
        when (buildingsSelfChanged obs obsPrev) $ do
            -- abilities /= abilitiesPrev then do
            agentChat "buildingsSelfChanged !!!: "
            agentResetGrid
        (queue', interruptedAbilities) <- processQueue queue ([], [])

        -- add phantom building from the planner queue
        agentUpdateGrid
            ( \g ->
                foldl
                    (\ga (u, p) -> gridPlace u p ga)
                    g
                    [(abilityToUnit (unitTraits si) . getCmd $ a, tilePos . getTarget $ a) | a <- queue']
            )
        let reservedResources = actionsCost si queue'
            interruptedOrders = abilityToUnit (unitTraits si) . getCmd <$> interruptedAbilities
        unless (null interruptedOrders) $
            agentChat ("interrupted: " ++ show interruptedOrders)

        (affordableActions, orders') <- splitAffordable (interruptedOrders ++ buildOrder) reservedResources
        -- trace ("affordableActions : " ++ (show . length $ affordableActions) ++ " orders': " ++ (show . length $ orders')) (return ())

        unless (null affordableActions) $ do
            agentChat ("scheduling: " ++ show affordableActions `dbg` ("!!! affordable " ++ show affordableActions))

        debugTexts
            [ ("planned " ++ show (getCmd a), defMessage & #x .~ getTarget a ^. #x & #y .~ getTarget a ^. #y & #z .~ 10)
            | a <- affordableActions
            ]
        command affordableActions
        if null orders'
            then do
                -- transit
                return $ BuildArmyAndWin obs
            else
                return $ BuildOrderExecutor orders' (queue' ++ affordableActions) obs abilities
    agentStep (BuildArmyAndWin obsPrev) = do
        -- debugUnitPos
        si <- agentStatic
        obs <- agentObs
        agentUpdateArmy obsPrev
        when (selfBuildingsCount obs /= selfBuildingsCount obsPrev) agentResetGrid
        reassignIdleProbes
        -- when (unitsChanged obs obsPrev) $ do
        --  agentPut (obs, gridUpdate obs (gridFromImage $ gameInfo si ^. (#startRaw . #placementGrid))) -- >> command [Chat $ pack "grid updated"]

        res <- runMaybeT buildPylons

        let idleGates = runC $ unitsSelf obs .| unitTypeC ProtossGateway .| unitIdleC
            idleRobos = runC $ unitsSelf obs .| unitTypeC ProtossRoboticsfacility .| unitIdleC
            gameLoop = obs ^. #gameLoop
        command [SelfCommand TrainImmortal robo | robo <- idleRobos]
        command [SelfCommand (if (gameLoop `div` 5) == 0 then TrainZealot else TrainStalker) gate | gate <- idleGates]

        trainProbes
        randomArmyFiddling

        return $ BuildArmyAndWin obs
