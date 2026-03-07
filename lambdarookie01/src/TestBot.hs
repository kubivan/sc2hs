{-# LANGUAGE BangPatterns #-}
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
import AgentBulidUtils
import ArmyLogic
import BotDynamicState
import Footprint (getFootprint)
import Observation
import Army.Army
import SC2.Geometry
import SC2.Grid
import SC2.Ids.AbilityId
import SC2.Ids.UnitTypeId
import SC2.Proto.Data (Race (..))
import SC2.Proto.Data qualified as Proto
import Squad
import Squad.Behavior
import Squad.State
import SC2.TechTree
import SC2.Utils
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
import Utils

import Conduit (concatC, filterC, findC, headC, lengthC, mapC, runConduitPure, sinkList, yieldMany, (.|))
import Control.Applicative (Alternative (..))
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Writer.Strict
import Control.Concurrent (forkIO)
import Data.Conduit.List qualified as CL
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List (find, partition, sortOn)
import Data.Maybe (catMaybes, fromJust, isJust, listToMaybe, mapMaybe)

import Data.ProtoLens (defMessage)
import Data.Sequence (Seq (..), empty, (|>))
import Data.Set qualified as Set
import Data.Word (Word32)
import Debug.Trace (trace, traceM)
import Lens.Micro (to, (&), (.~), (^.), (^..))
import Lens.Micro.Extras (view)
import Proto.S2clientprotocol.Raw_Fields (facing)
import SC2.Grid.Algo (regionGraphBfs)
import StepMonad (AsyncStaticInfo (..), StaticInfo (siAsyncStaticInfo), siRegionLookup, siRegionPathToEnemyResolved, siRegionsResolved)
import System.Random (newStdGen)
import SC2.Ids.UpgradeId (UpgradeId(Darktemplarblinkupgrade))

import Control.Concurrent.STM

deathBall :: [Tech]
deathBall = [TechUnit ProtossDarkTemplar, TechUpgrade Darktemplarblinkupgrade]

stepTowardsTechGoal :: (HasObs d, HasGrid d) => [Tech] -> StepMonad d ()
stepTowardsTechGoal goal = do
    (si, abilities) <- agentAsk

    obs <- agentObs
    let currentTechs =
            Set.fromList $
                runC $
                    unitsSelf obs
                        .| mapC (view #unitType)
                        .| mapC (TechUnit . toEnum')
        inProgress =
            Set.fromList $
                runC $
                    unitsSelf obs
                        .| mapC (view #orders)
                        .| concatC
                        .| mapC (view #abilityId)
                        .| CL.mapMaybe (fmap TechUnit . abilityToUnitSafe traits . toEnum')

        goal' = Set.fromList $ foldl' (\acc x -> acc ++ techPath HashMap.! x) [] goal
        traits = unitTraits si

        availTechs = Set.fromList $ map TechUnit $ catMaybes $ (abilityToUnitSafe traits) <$> HashMap.foldl' (++) [] abilities

        needToBuild = goal' `Set.difference` (currentTechs `Set.union` inProgress)
        pathTobuild = Set.toList $ availTechs `Set.intersection` needToBuild
    traceM $ "goal'" ++ show goal'
    traceM $ "needToBuild" ++ show needToBuild
    traceM $ "currentTechs" ++ show currentTechs
    traceM $ "availTech" ++ show availTechs
    traceM $ "Path towards goal" ++ show pathTobuild
    unless (null pathTobuild) $ do
        ds <- agentGet
        obs <- agentObs
        let toBuild = head pathTobuild
        case toBuild of
            TechUnit u -> do
                if isUnitStructure u then do
                    grid <- agentGrid
                    cres <- tryCreate grid (Cost 0 0) u
                    case cres of
                        Just (action, cost, grid') -> command [action]
                        _ -> return ()
                else do -- Unit training
                    let trainAbility = trainDeps HashMap.! u
                        producer = head $ runC $ unitsSelf obs
                            -- .| unitIdleC
                            .| unitTypeC (abilityExecutor HashMap.! trainAbility)
                    command [SelfCommand trainAbility [producer]]

            TechUpgrade upgrade -> do
                let upgradeAbility = researchDeps HashMap.! upgrade
                    producer = head $ runC $ unitsSelf obs
                        -- .| unitIdleC
                        .| unitTypeC (abilityExecutor HashMap.! upgradeAbility)
                command [SelfCommand upgradeAbility [producer]]
            _ -> error "not implemented"


type BuildOrder = [UnitTypeId]

data BotPhase
    = Opening
    | BuildOrderExecutor BuildOrder [Action] Observation UnitAbilities
    | BuildArmyAndWin Observation [Tech]

strBotPhase :: BotPhase -> String
strBotPhase (Opening) = "Opening"
strBotPhase (BuildOrderExecutor{}) = "BuildOrderExecutor"
strBotPhase (BuildArmyAndWin{}) = "BuildArmyAndWin Observation"

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

buildAction :: (HasObs d, HasGrid d) => UnitTypeId -> Grid -> Cost -> MaybeStepMonad d (Action, Cost, Grid)
buildAction ProtossAssimilator grid reservedRes = do
    (isAffordable, cost) <- lift $ canAfford ProtossAssimilator reservedRes
    guard isAffordable
    obs <- lift agentObs
    builder <- MaybeT . return $ findBuilder obs
    geyser <- MaybeT . return $ findFreeGeyser obs

    let res = UnitCommand PROTOSSBUILDASSIMILATOR [builder] geyser

    return (res, cost, grid) -- `Utils.dbg` ("building Assimilator target " ++ show geyser ++ " builder " ++ show builder ++ " putting to the grid!!!!")
buildAction order grid reservedRes = do
    enabled <- lift . inBuildThechTree $ order
    guard enabled -- `Utils.dbg` (show order ++ " enabled " ++ show enabled)
    (isAffordable, cost) <- lift $ canAfford order reservedRes
    guard isAffordable

    si <- lift agentStatic
    ds <- lift agentGet
    obs <- lift agentObs
    grid <- lift agentGrid
    let ability = unitToAbility (unitTraits si) order
        footprint = getFootprint order
    guard (isBuildAbility ability)
    builder <- MaybeT . return $ findBuilder obs
    pos <- MaybeT . return $ findPlacementPos obs (expandsPos si) grid (heightMap si) order
    let grid' = addMark grid footprint pos
        obs' = addOrder (builder ^. #tag) ability . addUnit order $ obs
    traceM $ (show order ++ " buildPos " ++ show pos ++ " builder " ++ show builder ++ " putting to the grid!!!!")
    -- or lift $ agentModify (obsL .~ obs' . gridL .~ grid')
    lift $ agentModifyObs (const obs')
    lift $ agentModifyGrid (const grid')

    let res = PointCommand ability [builder] (fromTuple pos)

    return (res, cost, grid') `Utils.dbg` ("builder orders " ++ show (builder ^. #orders))

distantEnough :: (Foldable t, Pointable p1, Pointable p2) => t p2 -> Float -> p1 -> Bool
distantEnough units radius pos = all (\p -> distSquared pos p >= radius * radius) units

pylonBuildAction :: (HasObs d, HasGrid d) => Grid -> Cost -> MaybeStepMonad d (Action, Cost, Grid)
pylonBuildAction grid reservedRes = do
    (isAffordable, cost) <- lift $ canAfford ProtossPylon reservedRes
    guard isAffordable

    si <- lift agentStatic
    ds <- lift agentGet
    obs <- lift agentObs
    let hasPylonsInProgress = not $ Prelude.null $ runC $ unitsSelf obs .| unitTypeC ProtossPylon .| filterC (\u -> u ^. #buildProgress < 1)
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
    let obs' = addOrder (builder ^. #tag) PROTOSSBUILDPYLON obs
    lift $ agentModify ((obsL .~ obs') . (gridL .~ grid'))
    return (PointCommand PROTOSSBUILDPYLON [builder] (fromTuple pylonPos), cost, grid') `Utils.dbg` ("pylonPos: " ++ show pylonPos)

createAction :: (HasObs d, HasGrid d) => Grid -> Cost -> UnitTypeId -> MaybeStepMonad d (Action, Cost, Grid)
createAction grid reserved order = do
    (isAffordable, cost) <- lift $ canAfford order reserved
    guard isAffordable -- `Utils.dbg` (show order ++ " affordable " ++ show isAffordable ++ " cost: " ++ show cost)
    buildAction order grid reserved <|> pylonBuildAction grid reserved

trainProbes :: (HasObs d) => StepMonad d ()
trainProbes = do
    obs <- agentObs
    let units = unitsSelf obs
        probeCount = runConduitPure $ units .| unitTypeC ProtossProbe .| lengthC
        assimCount = runConduitPure $ units .| unitTypeC ProtossAssimilator .| lengthC
        nexuses :: [Units.Unit]
        nexuses = runC $ units .| unitTypeC ProtossNexus .| filterC (\n -> (n ^. #buildProgress) == 1) .| unitIdleC
        nexusCount = length nexuses
        optimalCount = assimCount * 3 + nexusCount * 16
    when (optimalCount - probeCount > 0) $ command [SelfCommand NEXUSTRAINPROBE nexuses]

-- `Utils.dbg` ("trainProbes: optCount" ++ show optimalCount ++ " probes: " ++ show probeCount )

-- TODO: merge unitHasOrder && unitIsHarvesting
unitHasOrder :: AbilityId -> Units.Unit -> Bool
unitHasOrder order u = order `elem` orders
  where
    orders = toEnum' . view #abilityId <$> u ^. #orders

reassignIdleProbes :: (HasObs d) => StepMonad d ()
reassignIdleProbes = do
    obs <- agentObs
    let units = unitsSelf obs
        probes = units .| unitTypeC ProtossProbe

        mineralField =
            runConduitPure $
                probes
                    .| filterC (\p -> HARVESTGATHERPROBE `elem` map (\o -> toEnum' (o ^. #abilityId)) (p ^. #orders))
                    .| mapC (\harvester -> head $ filter (\o -> HARVESTGATHERPROBE == toEnum' (o ^. #abilityId)) (harvester ^. #orders))
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
                [ UnitCommand HARVESTGATHERPROBE [harvester] assimilator
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
                        [ UnitCommand HARVESTGATHERPROBE [harvester] (closestMineralTo nexus)
                        | (nexus, harvester) <- zip nexusesUnder probePool
                        ]
                else do
                    -- trace "staffing: just idle probes" (return ())
                    let closestMineral to = runConduitPure $ obsUnitsC obs .| filterC isMineral .| closestC to
                    -- TODO: obsolete, rewrite
                    command [UnitCommand HARVESTGATHERPROBE [idle] (fromJust $ mineralField <|> closestMineral idle) | idle <- idleWorkers]

buildPylons :: (HasObs d, HasGrid d) => MaybeStepMonad d ()
buildPylons = do
    obs <- lift agentObs
    grid <- lift agentGrid
    let foodCap = fromIntegral $ obs ^. (#playerCommon . #foodCap) -- `Utils.debug` ("minerals: " ++ show minerals)
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
                        .| filterC (unitHasOrder PROTOSSBUILDPYLON)

        expectedFoodCap = 8 * incompletePylonsCount + 8 * orderedPylonsCount

    -- TODO:
    guard (foodCap + expectedFoodCap - foodUsed < 2)
    (act, _, _) <- pylonBuildAction grid (Cost 0 0)
    lift $ command [act]

processQueue :: (HasObs d) => [Action] -> ([Action], [Action]) -> StepMonad d ([Action], [Action])
processQueue (a : as) (q', interrupted) = do
    obs <- agentObs
    case findAssignee obs a of
        Nothing -> processQueue as (q', interrupted ++ [a])
        Just u ->
            if fromEnum (getCmd a) `elem` (u ^. #orders ^.. traverse . (Proto.abilityId . to fromIntegral))
                then processQueue as (q' ++ [a], interrupted)
                else processQueue as (q', interrupted)
processQueue [] res = return res

--TODO: very old, reimplement normally in stepmonad manner
splitAffordable :: (HasObs d, HasGrid d) => BuildOrder -> Cost -> StepMonad d ([Action], BuildOrder)
splitAffordable bo reserved = agentGet >>= (\ds -> agentGrid >>= \grid -> go bo Data.Sequence.empty grid reserved) -- `Utils.dbg` ("splitAffordable " ++ show bo ++ " reserved" ++ show reserved)
  where
    go :: (HasObs d, HasGrid d) => BuildOrder -> Seq Action -> Grid -> Cost -> StepMonad d ([Action], BuildOrder)
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

tryCreate :: (HasObs d, HasGrid d) => Grid -> Cost -> UnitTypeId -> StepMonad d (Maybe (Action, Cost, Grid))
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
    let regionLookupMap = siRegionLookup si
        squads = armySquads . dsArmy $ ds
        hashArmy :: HashMap.HashMap UnitTag Unit
        hashArmy = armyUnits . dsArmy $ ds

        squadLeaderTags :: [UnitTag]
        squadLeaderTags = head . squadUnits <$> squads
        squadLeaders = getUnits squadLeaderTags hashArmy
    mapM_ debugSquad squads
    -- debugTexts [("s " ++ show (u ^. #tag) ++ " at " ++ show (HashMap.lookup (tilePos (u ^. #pos)) regionLookupMap) ++ " state " ++ show (armyUnitStateStr $ squadState s) ++ " " ++ show (u ^. #orders), u ^. #pos) | (u, s) <- zip squadLeaders squads]
    debugTexts [("s " ++ show (u ^. #tag) ++ " at " ++ show (HashMap.lookup (tilePos (u ^. #pos)) regionLookupMap) ++ " " ++ show (u ^. #facing), u ^. #pos) | (u, s) <- zip squadLeaders squads]

agentResetGrid :: (HasObs d, HasGrid d) => StepMonad d ()
agentResetGrid =
    {-# SCC "agentResetGrid" #-}
    do
        obs <- agentObs
        ds <- agentGet
        gridPlacementStart <- gridFromImage . view (#startRaw . #placementGrid) . gameInfo <$> agentStatic
        gridPathingStart <- gridFromImage . view (#startRaw . #pathingGrid) . gameInfo <$> agentStatic

        let gridMerged = gridMerge pixelIsRamp gridPlacementStart gridPathingStart
            grid' = gridUpdate obs gridMerged

        -- agentPut $ setGrid grid' ds
        agentModifyGrid (const grid')

agentUpdateGrid :: (HasGrid d) => (Grid -> Grid) -> StepMonad d ()
agentUpdateGrid f = 
  {-# SCC "agentUpdateGrid" #-}
  agentModifyGrid f

setArmy a st = st{dsArmy = a}

squadAssign :: Squad -> StepMonad BotDynamicState Bool
squadAssign s = do
    canSeek <- squadSeek s
    canExplore <- squadAssignToExplore' s
    return $ canSeek || canExplore

squadSeek :: Squad -> StepMonad BotDynamicState Bool
squadSeek squad = case squadState squad of
    SSEngageClose _ -> return True
    SSEngageFar _   -> return True
    _ -> do
        ds <- agentGet
        obs <- agentObs
        let unitByTag t = HashMap.lookup t (armyUnits (dsArmy ds))
            units = catMaybes $ [unitByTag t | t <- squadUnits squad]
            leader = head $ units
            closestEnemy = runConduitPure $ obsUnitsC obs .| filterC isEnemy .| closestC leader
        case closestEnemy of
            Nothing -> return False
            (Just enemy) -> do
                let squad' = squad{squadState = SSEngageFar (enemy ^. #tag)}
                    squads' = replaceSquad squad' (armySquads (dsArmy ds))
                    army' = (dsArmy ds){armySquads = squads'}
                agentPut $ setArmy army' ds
                return True

squadAssignToExploreBlind :: Squad -> StepMonad BotDynamicState Bool
squadAssignToExploreBlind squad = do
    ds <- agentGet
    si <- agentStatic
    let unitByTag t = HashMap.lookup t (armyUnits (dsArmy ds))
        units = catMaybes [unitByTag t | t <- squadUnits squad]
    case units of
        [] -> return False
        _ -> do
            command [PointCommand ATTACKATTACK units (toPoint2D (enemyStartLocation si))]
            return True

squadAssignToExplore :: Squad -> StepMonad BotDynamicState Bool
squadAssignToExplore s = do
    assigned <- isJust <$> runMaybeT (assignSegmented s)
    return assigned
  where
    assignSegmented :: Squad -> MaybeStepMonad BotDynamicState Bool
    assignSegmented squad = do
        ds <- lift agentGet
        si <- lift agentStatic

        regionsById <- MaybeT . pure $ siRegionsResolved si
        regionPathToEnemy <- MaybeT . pure $ siRegionPathToEnemyResolved si
        guard (not (null regionPathToEnemy))

        let allRegions = HashSet.fromList regionPathToEnemy
            usedRegions = HashSet.fromList $ mapMaybe squadAssignedRegion (armySquads (dsArmy ds))
            availableRegionIds = HashSet.toList $ allRegions `HashSet.difference` usedRegions
        lift $ traceM $ "assigning: all regions " ++ show allRegions
        lift $ traceM $ "assigning: used regions " ++ show usedRegions
        lift $ traceM $ "assigning: availableRegionIds " ++ show availableRegionIds

        rid <- MaybeT . pure $ listToMaybe availableRegionIds
        let region = regionsById HashMap.! rid
            squad' = squad{squadState = SSExploreRegion rid region}
            squads' = replaceSquad squad' (armySquads (dsArmy ds))
            army' = (dsArmy ds){armySquads = squads'}
        lift $ agentPut $ setArmy army' ds
        pure True

squadAssignToExplore' :: Squad -> StepMonad BotDynamicState Bool
squadAssignToExplore' s = do
    canExplore <- squadAssignToExplore s
    if canExplore then return True else squadAssignToExploreBlind s

agentAssignIdleSquads :: StepMonad BotDynamicState ()
agentAssignIdleSquads = do
    ds <- agentGet
    let army = dsArmy ds
        squads = armySquads army

    fullIdleSquads <- filter isSquadIdle <$> filterM isSquadFull squads
    mapM_ squadAssign fullIdleSquads

agentTryEngage :: StepMonad BotDynamicState ()
agentTryEngage = do
    ds <- agentGet
    let army = dsArmy ds
        squads = armySquads army

    mapM_ squadSeek squads

squadTransitionChooser :: TransitionChooser BotDynamicState
squadTransitionChooser squad oldState =
    case oldState of
        SSEngageFar _   -> retreat
        SSEngageClose _ -> retreat
        _               -> defaultTransitionChooser squad oldState
  where
    retreat = do
        obs <- agentObs
        let retreatPos = tilePos (findNexus obs ^. #pos)
        pure $ SSRetreat retreatPos

agentArmyControl :: StepMonad BotDynamicState ()
agentArmyControl = do
    agentAssignIdleSquads
    agentTryEngage
    ds <- agentGet
    let army = dsArmy ds
        squads = armySquads army
    squads' <- mapM (processSquadWith squadTransitionChooser) squads
    let army' = army{armySquads = squads'}
    agentPut $ setArmy army' ds

data Env = Env
  { chokeQueue :: TQueue ()
    , chokeVar :: TVar (Maybe AsyncStaticInfo)
  }

makeEnv :: IO Env
makeEnv = do
  queue <- newTQueueIO
  var   <- newTVarIO Nothing
  pure $ Env queue var

startChokeWorker :: Env -> Grid -> TilePos -> TilePos -> IO ()
startChokeWorker env grid startPos enemyPos =
  void . forkIO . forever $ do
    atomically $ readTQueue (chokeQueue env)
    let !(rays, gridChoked) = findAllChokePoints grid
        !regions = gridSegment gridChoked
        !regionsById = HashMap.fromList regions
        !regionLookup = complementRegionLookup (buildRegionLookup regions) (foldl' (++) [] rays)
        !regionGraph = buildRegionGraph regions regionLookup
        !pathToEnemy =
            maybe
                []
                (\(startRegion, enemyRegion) -> regionGraphBfs regionGraph startRegion enemyRegion)
                ((,) <$> HashMap.lookup startPos regionLookup <*> HashMap.lookup enemyPos regionLookup)
        !result =
            AsyncStaticInfo
                { asiRegionGraph = regionGraph
                , asiRegionLookup = regionLookup
                , asiRegions = regionsById
                , asiRegionPathToEnemy = pathToEnemy
                }
    atomically $ writeTVar (chokeVar env) (Just result)

data BotAgent
    = BotAgent
        { botPhase :: BotPhase
        , botStaticInfo :: StaticInfo
        , botDynState :: BotDynamicState
        , botEnv :: Env
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
            gridPathingClean = removeMark gridPathing (getFootprint ProtossNexus) (tilePos nexusPos)
            grid = gridMerge pixelIsRamp gridPlacement gridPathing
            nexusPos = view #pos $ head $ runC $ unitsSelf obsRaw .| unitTypeC ProtossNexus
            -- TODO: sort expands based on region connectivity: closest is not always the next one
            expands = sortOn (distSquared nexusPos) $ findExpands obsRaw grid heightMap
            enemyStart = tilePos $ enemyBaseLocation gi obsRaw

            playerInfos = gi ^. #playerInfo
            playerGameInfo = head $ filter (\gi -> gi ^. #playerId == playerId) playerInfos

            si = StaticInfo gi playerGameInfo unitTraits heightMap expands enemyStart Nothing

        env <- makeEnv
        startChokeWorker env gridPathingClean (tilePos nexusPos) enemyStart
        atomically $ writeTQueue (chokeQueue env) ()

        traceM "create ds"
        dynamicState <- makeDynamicState obsRaw grid
        traceM "created ds"
        return $ BotAgent Opening si dynamicState env

    agentRace _ = Protoss

    agentStep EmptyBotAgent _ _ = error ("agent FSM broken")
    agentStep (BotAgent phase si ds env) obs abilities = do
        asyncResult <- readTVarIO (chokeVar env)
        let si' = maybe si (\result -> si{siAsyncStaticInfo = Just result}) asyncResult
            sm = agentStepPhase phase
            (phase', plan, ds') = runStepM si' abilities ds { dsObs = obs ^. #observation } sm
            actions = obs ^. #actions
            errors = obs ^. #actionErrors
            tracedResult =
                if not (null actions) || not (null errors)
                    then -- trace ("taken actions " ++ show actions ++ "\nerrors " ++ show errors)
                        (BotAgent phase' si' ds' env, plan)
                    else (BotAgent phase' si' ds' env, plan)
        pure tracedResult

agentStepPhase :: BotPhase -> StepMonad BotDynamicState BotPhase
agentStepPhase Opening =
    {-# SCC "agentStep:Opening" #-}
    do
        obs <- agentObs

        agentModifyGrid (\g -> gridUpdate obs g)
        let nexus = findNexus obs
            fourGateBuild = [ProtossPylon, ProtossAssimilator, ProtossGateway, ProtossCyberneticsCore, ProtossAssimilator, ProtossGateway]
            expandBuild = [ProtossNexus, ProtossRoboticsFacility, ProtossGateway, ProtossGateway, ProtossAssimilator, ProtossAssimilator]

        command [SelfCommand NEXUSTRAINPROBE [nexus]]
        return $ BuildOrderExecutor (fourGateBuild ++ expandBuild) [] obs (HashMap.fromList [])
agentStepPhase (BuildOrderExecutor buildOrder queue obsPrev abilitiesPrev) =
    {-# SCC "agentStep:BuildOrderExecutor" #-}
    do
        debugUnitPos
        reassignIdleProbes
        trainProbes
        si <- agentStatic
        obs <- agentObs
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
                return $ BuildArmyAndWin obs deathBall
            else
                return $ BuildOrderExecutor orders' (queue' ++ affordableActions) obs abilities
agentStepPhase (BuildArmyAndWin obsPrev deathBall) =
    {-# SCC "agentStep:BuildArmyAndWin" #-}
    do
        si <- agentStatic
        obs <- agentObs
        agentUpdateArmy obsPrev
        debugSquads
        when (selfBuildingsCount obs /= selfBuildingsCount obsPrev) agentResetGrid
        reassignIdleProbes
        agentArmyControl
        stepTowardsTechGoal deathBall
        -- when (unitsChanged obs obsPrev) $ do
        --  agentPut (obs, gridUpdate obs (gridFromImage $ gameInfo si ^. (#startRaw . #placementGrid))) -- >> command [Chat $ pack "grid updated"]

        res <- runMaybeT buildPylons

        let idleGates = runC $ unitsSelf obs .| unitTypeC ProtossGateway .| unitIdleC
            idleRobos = runC $ unitsSelf obs .| unitTypeC ProtossRoboticsFacility .| unitIdleC
            gameLoop = obs ^. #gameLoop
        --command [SelfCommand ROBOTICSFACILITYTRAINIMMORTAL idleRobos]
        command [SelfCommand (if (gameLoop `div` 5) == 0 then GATEWAYTRAINZEALOT else GATEWAYTRAINSTALKER) idleGates]

        trainProbes

        return $ BuildArmyAndWin obs deathBall

-- --------------------------------
-- instance Agent TestBot BotDynamicState where
--     type DynamicState TestBot = BotDynamicState

--     agentStep :: a -> A.ResponseObservation -> UnitAbilities -> (a, StepPlan)

--     makeDynamicState :: Observation -> Grid -> IO BotDynamicState
--     makeDynamicState obs grid = do
--         gen <- newStdGen -- Initialize a new random generator
--         return $ BotDynamicState obs grid gen emptyArmy

--     agentRace _ = C.Protoss
