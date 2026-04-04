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
import Army.Army
import ArmyLogic
import BotDynamicState
import Footprint (getFootprint)
import Observation
import SC2.Geometry
import SC2.Grid
import SC2.Ids.AbilityId
import SC2.Ids.UnitTypeId
import SC2.Proto.Data (Race (..))
import SC2.Proto.Data qualified as Proto
import SC2.Spatial qualified as Spatial

import Intent hiding (unitHasOrder)
import PlanM
import SC2.TechTree
import SC2.Utils
import Squad
import Squad qualified as Squad
import Squad.Behavior
import Squad.State
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
import Control.Concurrent (forkIO)
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Writer.Strict
import Data.Conduit.List qualified as CL
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List (find, isPrefixOf, nub, partition, sortOn, (\\))
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust, isJust, listToMaybe, mapMaybe)

import Data.ProtoLens (defMessage)
import Data.Sequence (Seq (..), empty, (|>))
import Data.Set qualified as Set
import Data.Word (Word32)
import Debug.Trace (trace, traceM)
import Lens.Micro (to, (&), (.~), (^.), (^..))
import Lens.Micro.Extras (view)
import Proto.S2clientprotocol.Error qualified as E
import Proto.S2clientprotocol.Raw_Fields (facing)
import SC2.Grid.Algo (regionGraphBfs)
import SC2.Ids.UpgradeId (UpgradeId (Darktemplarblinkupgrade))
import StepMonad (AsyncStaticInfo (..), StaticInfo (siAsyncStaticInfo), siRegionLookup, siRegionPathToEnemyResolved, siRegionsResolved)
import StepMonadUtils (agentUnitCost)
import System.Random (newStdGen)

import Control.Concurrent.STM

deathBall :: [Tech]
deathBall = [TechUnit ProtossDarkTemplar, TechUpgrade Darktemplarblinkupgrade]

stepTowardsTechGoal :: (HasObs d, HasGrid d, HasBuildIntents d, HasReservedCost d) => [Tech] -> StepMonad d ()
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
        obs <- agentObs
        let toBuild = head pathTobuild
            simLoop = show $ obs ^. #gameLoop
        case toBuild of
            TechUnit u -> do
                if isUnitStructure u
                    then do
                        -- spawnIntent (IntentId ("step-towards-goal-build-" <> show (u))) (buildStructureIntent u)
                        spawnIntentUnique (IntentId ("step-towards-goal-build")) (buildStructureIntent u)
                    else do
                        -- Unit training
                        spawnIntentUnique (IntentId ("step-towards-goal-train")) (trainUnitIntent u)
            TechUpgrade upgrade -> do
                let upgradeAbility = researchDeps HashMap.! upgrade
                    producer =
                        head $
                            runC $
                                unitsSelf obs
                                    -- .| unitIdleC
                                    .| unitTypeC (abilityExecutor HashMap.! upgradeAbility)
                issueSelfCommandReserveAware upgradeAbility [producer]
            _ -> error "not implemented"

data BotPhase
    = Opening
    | BuildOrderExecutor BuildOrder Observation UnitAbilities
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

canAffordCopies :: (HasObs d, HasReservedCost d) => UnitTypeId -> Int -> StepMonad d Bool
canAffordCopies uid count = do
    obs <- agentObs
    reserved <- agentGetReservedCost
    unitCost <- agentUnitCost uid
    let totalCost = sum (replicate count unitCost)
    pure $ obsResources obs + reserved >= totalCost

issueSelfCommandReserveAware :: (HasObs d, HasReservedCost d) => AbilityId -> [Units.Unit] -> StepMonad d ()
issueSelfCommandReserveAware _ [] = pure ()
issueSelfCommandReserveAware ability executors = do
    si <- agentStatic
    case abilityToUnitSafe (unitTraits si) ability of
        Just uid -> do
            canAfford <- canAffordCopies uid (length executors)
            when canAfford $ command [SelfCommand ability executors]
        Nothing -> command [SelfCommand ability executors]

trainProbes :: (HasObs d, HasReservedCost d) => StepMonad d ()
trainProbes = do
    obs <- agentObs
    let units = unitsSelf obs
        probeCount = runConduitPure $ units .| unitTypeC ProtossProbe .| lengthC
        assimCount = runConduitPure $ units .| unitTypeC ProtossAssimilator .| lengthC
        nexuses :: [Units.Unit]
        nexuses = runC $ units .| unitTypeC ProtossNexus .| filterC (\n -> (n ^. #buildProgress) == 1 && null (n ^. #orders)) .| unitIdleC
        nexusCount = length nexuses
        optimalCount = assimCount * 3 + nexusCount * 16
    when (optimalCount - probeCount > 0) $ issueSelfCommandReserveAware NEXUSTRAINPROBE nexuses

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
                                    obsUnitsC obs .| filterC isMineral .| findC (\m -> Spatial.distManhattan (m ^. #pos) (nexus ^. #pos) <= 12)
                    command
                        [ UnitCommand HARVESTGATHERPROBE [harvester] (closestMineralTo nexus)
                        | (nexus, harvester) <- zip nexusesUnder probePool
                        ]
                else do
                    -- trace "staffing: just idle probes" (return ())
                    let closestMineral to = runConduitPure $ obsUnitsC obs .| filterC isMineral .| closestC to
                    -- TODO: obsolete, rewrite
                    command [UnitCommand HARVESTGATHERPROBE [idle] (fromJust $ mineralField <|> closestMineral idle) | idle <- idleWorkers]

buildPylons :: (HasObs d, HasGrid d, HasBuildIntents d, HasReservedCost d) => StepMonad d ()
buildPylons = do
    obs <- agentObs
    grid <- agentGrid
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
    when (foodCap + expectedFoodCap - foodUsed < 2) $
        spawnIntentUnique (IntentId "buildPylons") (buildStructureIntent ProtossPylon)

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

squadAssign :: Squad -> StepMonad BotDynamicState Bool
squadAssign s = do
    canSeek <- squadSeek s
    canExplore <- squadAssignToExplore' s
    return $ canSeek || canExplore

squadSeek :: Squad -> StepMonad BotDynamicState Bool
squadSeek squad = case squadState squad of
    SSEngage (FSEngageClose _) -> return True
    SSEngage (FSEngageFar _) -> return True
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
                let squad' = squad{squadState = SSEngage (FSEngageFar (enemy ^. #tag))}
                agentModifyArmy (\army -> army{armySquads = replaceSquad squad' (armySquads army)})
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
            command [PointCommand ATTACKATTACK units (fromTuple (enemyStartLocation si))]
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
            squad' = squad{squadState = SSExploreRegion (FSExploreRegion rid region)}
        lift $ agentModifyArmy (\army -> army{armySquads = replaceSquad squad' (armySquads army)})
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

agentArmyControl :: StepMonad BotDynamicState ()
agentArmyControl = do
    agentAssignIdleSquads
    agentTryEngage
    ds <- agentGet
    let army = dsArmy ds
        squads = armySquads army
    squads' <- mapM processSquad squads
    agentModifyArmy (\current -> current{armySquads = squads'})

data Env = Env
    { chokeQueue :: TQueue ()
    , chokeVar :: TVar (Maybe AsyncStaticInfo)
    }

makeEnv :: IO Env
makeEnv = do
    queue <- newTQueueIO
    var <- newTVarIO Nothing
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
    gen <- newStdGen
    return $ BotDynamicState obs grid (Cost 0 0) gen emptyArmy Map.empty

hasActiveBoIntent :: StepMonad BotDynamicState Bool
hasActiveBoIntent = do
    intents <- agentGetBuildIntents
    pure $ any isBoIntentId (Map.keys intents)
  where
    isBoIntentId (IntentId raw) = "bo-" `isPrefixOf` raw

actionErrorToPending :: Proto.ActionError -> Maybe PendingActionError
actionErrorToPending err =
    if rawTag == 0 || rawAbility == 0
        then Nothing
        else Just (PendingActionError (fromIntegral rawTag) (toEnum' (fromIntegral rawAbility)))
  where
    rawTag = err ^. #unitTag
    rawAbility = err ^. #abilityId

formatActionError :: Proto.ActionError -> String
formatActionError err =
    "{unitTag="
        <> show rawTag
        <> ", abilityId="
        <> show rawAbility
        <> " ("
        <> abilityName
        <> "), result="
        <> show (fromEnum rawResult)
        <> " ("
        <> resultName
        <> ")}"
  where
    rawTag = err ^. #unitTag
    rawAbility = err ^. #abilityId
    rawResult = err ^. #result
    abilityName = if rawAbility == 0 then "UnknownAbility" else show (toEnum' (fromIntegral rawAbility) :: AbilityId)
    resultName = show (rawResult :: E.ActionResult)

formatActionErrors :: [Proto.ActionError] -> String
formatActionErrors errs = show (map formatActionError errs)

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
            expands = sortOn (Spatial.distSquared nexusPos) $ findExpands obsRaw grid heightMap
            enemyStart = Spatial.toTilePos $ enemyBaseLocation gi obsRaw

            playerInfos = gi ^. #playerInfo
            playerGameInfo = head $ filter (\gi -> gi ^. #playerId == playerId) playerInfos

            si = StaticInfo gi playerGameInfo unitTraits heightMap expands (tilePos nexusPos) enemyStart Nothing

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
        let errors = obs ^. #actionErrors
            pendingErrors = mapMaybe actionErrorToPending errors
            si' = maybe si (\result -> si{siAsyncStaticInfo = Just result}) asyncResult
            sm = agentStepPhase phase
            dsPrepared = ds{dsObs = obs ^. #observation}
            (phase', plan, ds') = runStepM si' abilities dsPrepared sm
            actions = obs ^. #actions
            tracedResult =
                if not (null actions) || not (null errors)
                    then (BotAgent phase' si' ds' env, plan)
                    else (BotAgent phase' si' ds' env, plan)
        when (not . null $ obs ^. #actionErrors) $ traceM $ "!!!errors: " <> formatActionErrors (obs ^. #actionErrors)
        when (not . null $ obs ^. #actions) $ traceM $ "!!!actions: " <> (show $ obs ^. #actions)
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

        issueSelfCommandReserveAware NEXUSTRAINPROBE [nexus]
        return $ BuildOrderExecutor (boFromUnits (fourGateBuild ++ expandBuild)) obs (HashMap.fromList [])
agentStepPhase (BuildOrderExecutor buildOrder obsPrev abilitiesPrev) =
    {-# SCC "agentStep:BuildOrderExecutor" #-}
    do
        debugUnitPos
        reassignIdleProbes
        obs <- agentObs
        abilities <- agentAbilities
        when (buildingsSelfChanged obs obsPrev) $ do
            agentResetGrid
        buildPylons
        outcomes <- intentEngine
        buildOrder' <- runBO outcomes buildOrder
        boIntentActive <- hasActiveBoIntent
        unless boIntentActive trainProbes
        if null buildOrder'
            then do
                -- transit
                return $ BuildArmyAndWin obs deathBall
            else
                return $ BuildOrderExecutor buildOrder' obs abilities
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
        outcomes <- intentEngine
        -- when (unitsChanged obs obsPrev) $ do
        --  agentPut (obs, gridUpdate obs (gridFromImage $ gameInfo si ^. (#startRaw . #placementGrid))) -- >> command [Chat $ pack "grid updated"]

        buildPylons

        let idleGates = runC $ unitsSelf obs .| unitTypeC ProtossGateway .| unitIdleC
            idleRobos = runC $ unitsSelf obs .| unitTypeC ProtossRoboticsFacility .| unitIdleC
            gameLoop = obs ^. #gameLoop
        -- command [SelfCommand ROBOTICSFACILITYTRAINIMMORTAL idleRobos]
        issueSelfCommandReserveAware (if (gameLoop `div` 5) == 0 then GATEWAYTRAINZEALOT else GATEWAYTRAINSTALKER) idleGates

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
