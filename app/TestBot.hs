{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TestBot where

import Actions
import Agent
import SC2.Army.Army
import SC2.Squad
import SC2.Squad.State
import SC2.Squad.Behavior
import SC2.Squad.FSExploreRegion
import SC2.Utils
import AgentBulidUtils
import ArmyLogic
import BotDynamicState
import Footprint (getFootprint)
import SC2.Grid
import Utils
import Observation
import SC2.Proto.Data (Race (..))
import SC2.Proto.Data qualified as Proto
import SC2.Ids.AbilityId
import SC2.Ids.UnitTypeId
import StepMonad
import Units (
    Unit,
    closestC,
    isAssimilator,
    isMineral,
    mapTilePosC,
    runC,
    toEnum',
    unitIdleC,
    unitTypeC,
 )
import SC2.Geometry

import Conduit (filterC, findC, headC, lengthC, mapC, runConduitPure, sinkList, yieldMany, (.|))
import Control.Applicative (Alternative (..))
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Writer.Strict
import Data.Conduit.List qualified as CL
import Data.Foldable (toList)
import Data.HashSet qualified as HashSet
import Data.List (partition, sortOn)
import Data.Maybe (mapMaybe)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.ProtoLens (defMessage)
import Data.Sequence (Seq (..), empty, (|>))
import Data.Word (Word32)
import Debug.Trace (traceM, trace)
import Lens.Micro (to, (&), (.~), (^.), (^..))
import Lens.Micro.Extras (view)
import System.Random (newStdGen)

type BuildOrder = [UnitTypeId]

data BotPhase
    = Opening
    | BuildOrderExecutor BuildOrder [Action] Observation UnitAbilities
    | BuildArmyAndWin Observation

strBotPhase :: BotPhase -> String
strBotPhase (Opening) = "Opening"
strBotPhase (BuildOrderExecutor{}) = "BuildOrderExecutor"
strBotPhase (BuildArmyAndWin _) = "BuildArmyAndWin Observation"

unitsData :: [Proto.UnitTypeData] -> UnitTraits
unitsData raw =
    HashMap.fromList . runConduitPure $
        yieldMany raw
            .| mapC (\a -> (toEnum . fromIntegral $ a ^. #unitId, a))
            .| sinkList

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

    let res = UnitCommand BuildAssimilator [builder] geyser

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

    let res = PointCommand ability [builder] (fromTuple pos)

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
    let obs' = addOrder (builder ^. #tag) BuildPylon obs
    lift . agentPut $ dsUpdate obs' grid' ds
    return (PointCommand BuildPylon [builder] (fromTuple pylonPos), cost, grid') `Utils.dbg` ("pylonPos: " ++ show pylonPos)

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
    when (optimalCount - probeCount > 0) $ command [SelfCommand TrainProbe nexuses]

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
                [ UnitCommand HarvestGatherProbe [harvester] assimilator
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
                        [ UnitCommand HarvestGatherProbe [harvester] (closestMineralTo nexus)
                        | (nexus, harvester) <- zip nexusesUnder probePool
                        ]
                else do
                    -- trace "staffing: just idle probes" (return ())
                    let closestMineral to = runConduitPure $ obsUnitsC obs .| filterC isMineral .| closestC to
                    -- TODO: obsolete, rewrite
                    command [UnitCommand HarvestGatherProbe [idle] (fromJust $ mineralField <|> closestMineral idle) | idle <- idleWorkers]

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
            if fromEnum (getCmd a) `elem` (u ^. #orders ^.. traverse . (Proto.abilityId . to fromIntegral))
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

debugUnitPos :: WriterT StepPlan (StateT BotDynamicState (Reader (StaticInfo, UnitAbilities))) ()
debugUnitPos = agentObs >>= \obs -> debugTexts [("upos " ++ show (tilePos . view #pos $ c), c ^. #pos) | c <- runC $ unitsSelf obs]

getUnits :: [UnitTag] -> HashMap.HashMap UnitTag Unit -> [Unit]
getUnits tags hashArmy = catMaybes $ (\t -> HashMap.lookup t hashArmy) <$> tags

debugSquads :: StepMonad BotDynamicState ()
debugSquads = do
    -- agentObs >>= \obs -> debugTexts [("squad " ++ show (), c ^. #pos) | c <- runC $ unitsSelf obs]
    ds <- agentGet
    (si, _) <- agentAsk
    let regionLookupMap = regionLookup si
        squads = armySquads . dsArmy $ ds
        hashArmy :: HashMap.HashMap UnitTag Unit
        hashArmy = armyUnits . dsArmy $ ds

        squadLeaderTags :: [UnitTag]
        squadLeaderTags = head . squadUnits <$> squads
        squadLeaders = getUnits squadLeaderTags hashArmy
    -- debugTexts [("s " ++ show (u ^. #tag) ++ " at " ++ show (HashMap.lookup (tilePos (u ^. #pos)) regionLookupMap) ++ " state " ++ show (armyUnitStateStr $ squadState s) ++ " " ++ show (u ^. #orders), u ^. #pos) | (u, s) <- zip squadLeaders squads]
    debugTexts [("s " ++ show (u ^. #tag) ++ " at " ++ show (HashMap.lookup (tilePos (u ^. #pos)) regionLookupMap) ++ " " ++ show (u ^. #orders), u ^. #pos) | (u, s) <- zip squadLeaders squads]

agentResetGrid :: (AgentDynamicState d) => StepMonad d ()
agentResetGrid =
    {-# SCC "agentResetGrid" #-}
    do
        obs <- agentObs
        ds <- agentGet
        gridPlacementStart <- gridFromImage . view (#startRaw . #placementGrid) . gameInfo <$> agentStatic
        gridPathingStart <- gridFromImage . view (#startRaw . #pathingGrid) . gameInfo <$> agentStatic

        let gridMerged = gridMerge pixelIsRamp gridPlacementStart gridPathingStart
            grid' = gridUpdate obs gridMerged

        agentPut $ setGrid grid' ds

agentUpdateGrid :: (AgentDynamicState dyn) => (Grid -> Grid) -> StepMonad dyn ()
agentUpdateGrid f =
    {-# SCC "agentUpdateGrid" #-}
    do
        ds <- agentGet
        agentPut $ setGrid (f (getGrid ds)) ds


setArmy a st = st { dsArmy = a }

squadAssign :: Squad -> StepMonad BotDynamicState ()
squadAssign s = do
    ds <- agentGet
    (si, _) <- agentAsk
    let regionById rid = siRegions si HashMap.! rid
        allRegions = HashMap.keysSet $ siRegions si
        usedRegions = HashSet.fromList $ mapMaybe squadAssignedRegion (armySquads (dsArmy ds))
        availableRegionIds = HashSet.toList $ allRegions `HashSet.difference` usedRegions
    traceM $ "assigning: all regions " ++ show (allRegions)
    traceM $ "assigning: used regions " ++ show (usedRegions)
    traceM $ "assigning: availableRegionIds " ++ show (availableRegionIds)
    case availableRegionIds of
        [] -> return () -- no unassigned regions left
        (rid : _) -> do
            -- Assign squad to region
            let squad' = s{squadState = wrapState (FSExploreRegion rid (regionById rid))}
                squads' = replaceSquad squad' (armySquads (dsArmy ds))
                army' = (dsArmy ds){armySquads = squads'}
            -- traceM $ "   assigded squads': " ++ show squad'
            agentPut $ setArmy army' ds

agentAssignIdleSquads ::  StepMonad BotDynamicState ()
agentAssignIdleSquads = do
    ds <- agentGet
    let army = dsArmy ds
        squads = armySquads army

    fullIdleSquads <- filter isSquadIdle <$> filterM isSquadFull squads
    mapM_ squadAssign fullIdleSquads

agentArmyControl :: StepMonad BotDynamicState ()
agentArmyControl = do
    agentAssignIdleSquads
    ds <- agentGet
    let army = dsArmy ds
        squads = armySquads army
    squads' <- mapM processSquad squads
    let army' = army{armySquads=squads'}
    agentPut $ setArmy army' ds

data BotAgent
    = BotAgent
        { botPhase :: BotPhase
        , botStaticInfo :: StaticInfo
        , botDynState :: BotDynamicState
        }
    | EmptyBotAgent

makeDynamicState :: Observation -> Grid -> IO BotDynamicState
makeDynamicState obs grid = do
    gen <- newStdGen -- Initialize a new random generator
    return $ BotDynamicState obs grid gen emptyArmy

instance Agent BotAgent where
    makeAgent :: BotAgent -> Word32 -> Proto.ResponseGameInfo -> Proto.ResponseData -> Proto.ResponseObservation -> IO BotAgent
    makeAgent _ playerId gi gameDataResp obs0 = do
        let heightMap = gridFromImage $ gi ^. #startRaw . #terrainHeight
            obsRaw = obs0 ^. #observation
            unitTraits = unitsData $ gameDataResp ^. #units
            gridPlacement = gridFromImage $ gi ^. #startRaw . #placementGrid
            gridPathing = gridFromImage $ gi ^. #startRaw . #pathingGrid
            grid = gridMerge pixelIsRamp gridPlacement gridPathing
            nexusPos = view #pos $ head $ runC $ unitsSelf obsRaw .| unitTypeC ProtossNexus
            -- TODO: sort expands based on region connectivity: closest is not always the next one
            expands = sortOn (distSquared nexusPos) $ findExpands obsRaw grid heightMap
            enemyStart = tilePos $ enemyBaseLocation gi obsRaw

            playerInfos = gi ^. #playerInfo
            playerGameInfo = head $ filter (\gi -> gi ^. #playerId == playerId) playerInfos

            (rays, gridChoked) = findAllChokePoints gridPathing
            regions = gridSegment gridChoked
            regionGraph = buildRegionGraph regions
            regionLookup = buildRegionLookup regions
            si = StaticInfo gi playerGameInfo unitTraits heightMap expands enemyStart regionGraph regionLookup (HashMap.fromList regions)

        traceM "create ds"
        dynamicState <- makeDynamicState obsRaw grid
        traceM "created ds"
        return $ BotAgent Opening si dynamicState

    agentRace _ = Protoss

    agentStep EmptyBotAgent _ _ = error ("agent FSM broken")
    agentStep (BotAgent phase si ds) obs abilities =

        let sm = agentStepPhase phase
            (phase', plan, ds') = runStepM si abilities (setObs (obs ^. #observation) ds) sm
            actions = obs ^. #actions
            errors = obs ^. #actionErrors
            tracedResult =
                if not (null actions) || not (null errors)
                then trace ("taken actions " ++ show actions ++ "\nerrors " ++ show errors)
                    (BotAgent phase' si ds', plan)
                else (BotAgent phase' si ds', plan)
        in tracedResult

agentStepPhase :: BotPhase -> StepMonad BotDynamicState BotPhase
agentStepPhase Opening =
    {-# SCC "agentStep:Opening" #-}
    do
        si <- agentStatic
        ds <- agentGet

        let gi = gameInfo si
            obs = getObs ds
            grid = getGrid ds
        agentPut $ setGrid (gridUpdate obs grid) ds
        let nexus = findNexus obs
            fourGateBuild = [ProtossPylon, ProtossAssimilator, ProtossGateway, ProtossCyberneticscore, ProtossAssimilator, ProtossGateway]
            expandBuild = [ProtossNexus, ProtossRoboticsfacility, ProtossGateway, ProtossGateway]

        command [SelfCommand TrainProbe [nexus]]
        return $ BuildOrderExecutor (fourGateBuild ++ expandBuild) [] obs (HashMap.fromList [])
agentStepPhase (BuildOrderExecutor buildOrder queue obsPrev abilitiesPrev) =
    {-# SCC "agentStep:BuildOrderExecutor" #-}
    do
        debugUnitPos
        reassignIdleProbes
        trainProbes
        si <- agentStatic
        ds <- agentGet
        let obs = getObs ds
        abilities <- agentAbilities
        when (buildingsSelfChanged obs obsPrev) $ do
            -- abilities /= abilitiesPrev then do
            -- agentChat "buildingsSelfChanged !!!: "
            agentResetGrid
        (queue', interruptedAbilities) <- processQueue queue ([], [])

        -- add phantom building from the planner queue
        agentUpdateGrid
            ( \g ->
                foldl
                    (\ga (u, p) -> gridPlace ga u p)
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
agentStepPhase (BuildArmyAndWin obsPrev) =
    {-# SCC "agentStep:BuildArmyAndWin" #-}
    do
        si <- agentStatic
        obs <- agentObs
        agentUpdateArmy obsPrev
        debugSquads
        when (selfBuildingsCount obs /= selfBuildingsCount obsPrev) agentResetGrid
        reassignIdleProbes
        agentArmyControl
        -- when (unitsChanged obs obsPrev) $ do
        --  agentPut (obs, gridUpdate obs (gridFromImage $ gameInfo si ^. (#startRaw . #placementGrid))) -- >> command [Chat $ pack "grid updated"]

        res <- runMaybeT buildPylons

        let idleGates = runC $ unitsSelf obs .| unitTypeC ProtossGateway .| unitIdleC
            idleRobos = runC $ unitsSelf obs .| unitTypeC ProtossRoboticsfacility .| unitIdleC
            gameLoop = obs ^. #gameLoop
        command [SelfCommand TrainImmortal idleRobos]
        command [SelfCommand (if (gameLoop `div` 5) == 0 then TrainZealot else TrainStalker) idleGates]

        trainProbes
        -- randomArmyFiddling

        return $ BuildArmyAndWin obs

-- --------------------------------
-- instance Agent TestBot BotDynamicState where
--     type DynamicState TestBot = BotDynamicState

--     agentStep :: a -> A.ResponseObservation -> UnitAbilities -> (a, StepPlan)

--     makeDynamicState :: Observation -> Grid -> IO BotDynamicState
--     makeDynamicState obs grid = do
--         gen <- newStdGen -- Initialize a new random generator
--         return $ BotDynamicState obs grid gen emptyArmy

--     agentRace _ = C.Protoss
