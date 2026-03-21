module Intent where

import Actions (Action (PointCommand, SelfCommand, UnitCommand), UnitTag)
import Conduit (filterC, (.|))
import Data.Function (on)
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HashMap
import Data.List (find, sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, isNothing)
import Data.Set qualified as Set
import Data.Word (Word32)
import Debug.Trace (trace, traceM)
import Footprint (getFootprint)
import Lens.Micro (Lens', (%~), (.~), (^.))
import Lens.Micro.Extras
import Observation (Cost (..), Observation, findNexus, getUnit, obsResources, obsUnitsC, unitsSelf)
import SC2.Geometry (fromTuple)
import SC2.Grid (Grid, TilePos, addMark, canPlaceBuilding, findPlacementPoint, findPlacementPointInRadius, removeMark, tilePos)
import SC2.Ids.AbilityId
import SC2.Ids.UnitTypeId (UnitTypeId (ProtossAssimilator, ProtossNexus, ProtossProbe, ProtossPylon))
import SC2.Spatial qualified as Spatial
import SC2.TechTree (abilityExecutor, unitToAbility)
import StepMonad
import StepMonadUtils (abilityAvailableForUnit, agentCanAfford, agentCanAffordWith, agentUnitCost)
import Target (Target (..))
import Units (Unit, isGeyser, mapTilePosC, runC, toEnum', unitTypeC)

newtype IntentId = IntentId [Char]
    deriving (Eq, Ord, Show)

data IntentStatus
    = IntentRunning
    | IntentCompleted
    | IntentFailed
    | IntentNeedsPrerequisite UnitTypeId
    deriving (Eq, Show)

data BuildStructurePhase
    = BSReserving
    | BSWaitResources Cost
    | BSGathering Cost
    | BSIssued Cost UnitTag Target Word32
    | BSAccepted UnitTag Target Word32
    | BSMonitoring UnitTag Target
    deriving (Show)

data TrainUnitPhase
    = TUWaiting
    | TUGathering Cost
    deriving (Show)

{- | Declarative intent composition via andThen (fail-fast sequential) and orElse (fallback-on-failure).

Production usage pattern in stepTowardsTechGoal or build-order sequencing:
  buildPylonIntent `andThen` buildAssimilatorIntent `orElse` buildProbeIntent

Semantics:
  - andThen: left completes → run right; left fails/timeout → fail parent (fail-fast)
  - orElse: primary fails → run fallback; primary succeeds → return success (never switches on NeedPrerequisite)
  - Cleanup: recursive walkthrough combinators; only active branch cleaned (prevents double-cleanup with reserves)
  - Timeout: uniform across all intents via buildPhaseTimedOut (build-phase only, no train timeout)
  - Status propagation: NeedPrerequisite blocks both branches; running propagates from active branch
-}
data IntentProgram d
    = PBuildStructure UnitTypeId BuildStructurePhase
    | PTrainUnit UnitTypeId TrainUnitPhase
    | PAndThen (IntentProgram d) (IntentProgram d)
    | POrElse (IntentProgram d) (IntentProgram d)
    deriving (Show)

data IntentTickResult d
    = Continue (IntentRuntime d)
    | Block (IntentRuntime d)
    | Done (IntentRuntime d) IntentStatus
    | NeedPrerequisite (IntentRuntime d) UnitTypeId

-- A single tick is a small transition algebra: stay blocked until next frame,
-- continue immediately in the current frame, finish, or request a prerequisite.
-- This fits behavior-tree style sequence/fallback composition better than Monad.
data StepResult next
    = StepBlock next
    | StepContinue next
    | StepDone IntentStatus
    | StepNeed UnitTypeId

instance Functor StepResult where
    fmap f result =
        case result of
            StepBlock next -> StepBlock (f next)
            StepContinue next -> StepContinue (f next)
            StepDone status -> StepDone status
            StepNeed uid -> StepNeed uid

type IntentStepResult d = StepResult (IntentProgram d)
type BuildPhaseStepResult = StepResult BuildStructurePhase
type TrainPhaseStepResult = StepResult TrainUnitPhase

data PendingActionError = PendingActionError
    { paeUnitTag :: UnitTag
    , paeAbilityId :: AbilityId
    }
    deriving (Eq, Show)

data IssuedCommand = IssuedCommand
    { icIntentId :: IntentId
    , icFrame :: Word32
    , icUnitTag :: UnitTag
    , icAbilityId :: AbilityId
    }
    deriving (Eq, Show)

data IntentRuntime d = IntentRuntime
    { intentId :: IntentId
    , intentProgram :: IntentProgram d
    , intentStartedFrame :: Word32
    }

advanceRuntime :: IntentProgram d -> IntentRuntime d -> IntentRuntime d
advanceRuntime nextProgram runtime = runtime{intentProgram = nextProgram}

liftIntentStepResult :: IntentRuntime d -> IntentStepResult d -> IntentTickResult d
liftIntentStepResult runtime stepResult =
    case stepResult of
        StepBlock nextProgram -> Block (advanceRuntime nextProgram runtime)
        StepContinue nextProgram -> Continue (advanceRuntime nextProgram runtime)
        StepDone status -> Done runtime status
        StepNeed uid -> NeedPrerequisite runtime uid

type IntentStore d = Map IntentId (IntentRuntime d)
type IntentOutcomeStore = Map IntentId IntentStatus

class HasBuildIntents d where
    buildIntentsL :: Lens' d (IntentStore d)

buildStartTimeoutFrames :: Word32
buildStartTimeoutFrames = 112

maxIntentTransitionsPerFrame :: Int
maxIntentTransitionsPerFrame = 8

feedbackRetentionFrames :: Word32
feedbackRetentionFrames = 32

buildStructureIntent :: UnitTypeId -> IntentProgram d
buildStructureIntent uid = PBuildStructure uid BSReserving

trainUnitIntent :: UnitTypeId -> IntentProgram d
trainUnitIntent uid = PTrainUnit uid TUWaiting

andThen :: IntentProgram d -> IntentProgram d -> IntentProgram d
andThen = PAndThen

orElse :: IntentProgram d -> IntentProgram d -> IntentProgram d
orElse = POrElse

matchIntentFromError :: [IssuedCommand] -> PendingActionError -> Maybe IntentId
matchIntentFromError issued err =
    icIntentId <$> find (isMatch err) (reverse issued)
  where
    isMatch e ic = paeUnitTag e == icUnitTag ic && paeAbilityId e == icAbilityId ic

releaseLandTarget :: (HasGrid d) => UnitTypeId -> Target -> StepMonad d ()
releaseLandTarget uid (TargetPos pos) =
    agentModifyGrid (\grid -> removeMark grid (getFootprint uid) pos)
releaseLandTarget _ (TargetUnit _) = pure ()

cleanupIntent :: (HasGrid d, HasReservedCost d) => IntentProgram d -> StepMonad d ()
cleanupIntent (PBuildStructure uid phase) =
    case phase of
        BSReserving -> pure ()
        BSWaitResources cost -> agentModifyReservedCost (+ cost)
        BSGathering cost -> agentModifyReservedCost (+ cost)
        BSIssued cost _ target _ -> do
            agentModifyReservedCost (+ cost)
            releaseLandTarget uid target
        BSAccepted _ target _ -> releaseLandTarget uid target
        BSMonitoring _ target -> releaseLandTarget uid target
cleanupIntent (PTrainUnit _ phase) =
    case phase of
        TUWaiting -> pure ()
        -- Training reserve is released eagerly when the train command is issued.
        TUGathering _ -> pure ()
cleanupIntent (PAndThen left _) = cleanupIntent left
cleanupIntent (POrElse primary _) = cleanupIntent primary

stepBlockResult :: next -> StepMonad d (StepResult next)
stepBlockResult = pure . StepBlock

stepContinueResult :: next -> StepMonad d (StepResult next)
stepContinueResult = pure . StepContinue

stepDoneResult :: IntentStatus -> StepMonad d (StepResult next)
stepDoneResult = pure . StepDone

stepNeedResult :: UnitTypeId -> StepMonad d (StepResult next)
stepNeedResult = pure . StepNeed

stepBlock :: IntentProgram d -> StepMonad d (IntentStepResult d)
stepBlock = stepBlockResult

stepContinue :: IntentProgram d -> StepMonad d (IntentStepResult d)
stepContinue = stepContinueResult

stepDone :: IntentStatus -> StepMonad d (IntentStepResult d)
stepDone = stepDoneResult

stepNeed :: UnitTypeId -> StepMonad d (IntentStepResult d)
stepNeed = stepNeedResult

andThenStep :: IntentProgram d -> IntentStepResult d -> IntentStepResult d
andThenStep right leftStep =
    case leftStep of
        StepDone IntentCompleted -> StepContinue right
        StepDone status -> StepDone status
        StepNeed uid -> StepNeed uid
        _ -> fmap (`PAndThen` right) leftStep

orElseStep :: IntentProgram d -> IntentStepResult d -> IntentStepResult d
orElseStep fallback primaryStep =
    case primaryStep of
        StepDone IntentFailed -> StepContinue fallback
        StepDone status -> StepDone status
        StepNeed uid -> StepNeed uid
        _ -> fmap (`POrElse` fallback) primaryStep

-- processActionErrorFeedback ::
--     (HasObs d, HasBuildIntents d, HasReservedCost d, HasGrid d) =>
--     StepMonad d IntentOutcomeStore
-- processActionErrorFeedback = do
--     frame <- (^. #gameLoop) <$> agentObs
--     ds <- agentGet
--     let errors = ds ^. pendingActionErrorsL
--         issued = ds ^. issuedCommandsL
--         matchedIntentIds = catMaybes (map (matchIntentFromError issued) errors)
--         prunedIssued = filter (not . tooOld frame) issued
--
--     agentModify (pendingActionErrorsL .~ [])
--     agentModify (issuedCommandsL .~ prunedIssued)
--
--     Map.fromList . catMaybes <$> mapM failIntentById matchedIntentIds
--   where
--     tooOld current ic = current >= icFrame ic && current - icFrame ic > feedbackRetentionFrames
--
--     failIntentById iid = do
--         current <- lookupIntent iid
--         case current of
--             Nothing -> pure Nothing
--             Just runtime -> do
--                 traceM ("[feedback] action error for " ++ show iid ++ ", failing intent")
--                 cleanupIntent (intentProgram runtime)
--                 removeIntent iid
--                 pure $ Just (iid, IntentFailed)

findProducerTag :: (HasObs d) => UnitTypeId -> StepMonad d (Maybe UnitTag)
findProducerTag producerType = do
    obs <- agentObs
    pure $ (^. #tag) <$> find ((== 1) . (^. #buildProgress)) (runC $ unitsSelf obs .| unitTypeC producerType)

agentFindBuilder :: (HasObs d) => StepMonad d (Maybe Unit)
agentFindBuilder = findBuilder <$> agentObs

findBuilder :: Observation -> Maybe Unit
findBuilder obs =
    find availableProbe (runC $ unitsSelf obs .| unitTypeC ProtossProbe)
  where
    availableProbe :: Unit -> Bool
    availableProbe unit =
        Prelude.null (unit ^. #orders)
            || (length (unit ^. #orders) == 1 && HARVESTGATHERPROBE `elem` map (toEnum' . (^. #abilityId)) (unit ^. #orders))

findPlacementPos :: Observation -> [TilePos] -> Grid -> Grid -> UnitTypeId -> Maybe TilePos
findPlacementPos _ expands grid gridHeight ProtossNexus = find (\pos -> canPlaceBuilding grid gridHeight pos (getFootprint ProtossNexus)) expands
findPlacementPos obs _ grid gridHeight ProtossPylon =
    findPlacementPoint grid gridHeight (getFootprint ProtossPylon) nexusPos (const True)
  where
    nexusPos = tilePos $ findNexus obs ^. #pos
findPlacementPos obs _ grid gridHeight uid = go pylons
  where
    go (p : ps) =
        case findPlacementPointInRadius grid gridHeight (getFootprint uid) p 6.5 of
            Just res -> Just res
            Nothing -> go ps
    go [] = Nothing
    pylons =
        runC $
            unitsSelf obs
                .| unitTypeC ProtossPylon
                .| mapTilePosC

findFreeGeyser :: Observation -> Maybe Unit
findFreeGeyser obs = find (\u -> not (tilePos (u ^. #pos) `Set.member` assimilatorPositions)) geysersSorted
  where
    assimilatorPositions = Set.fromList $ runC $ unitsSelf obs .| unitTypeC ProtossAssimilator .| mapTilePosC
    nexusPos = tilePos $ findNexus obs ^. #pos
    geysersSorted =
        sortBy
            (compare `on` (\u -> Spatial.distSquared u nexusPos))
            (runC $ obsUnitsC obs .| filterC isGeyser)

findPlacementTarget :: (HasObs d, HasGrid d) => UnitTypeId -> StepMonad d (Maybe Target)
findPlacementTarget uid = do
    obs <- agentObs
    grid <- agentGrid
    si <- agentStatic
    pure $
        if uid == ProtossAssimilator
            then TargetUnit <$> findFreeGeyser obs
            else TargetPos <$> findPlacementPos obs (expandsPos si) grid (heightMap si) uid

commandBuild ::
    (HasObs d, HasGrid d) =>
    Unit ->
    UnitTypeId ->
    Target ->
    StepMonad d ()
commandBuild builder uid target = do
    si <- agentStatic
    let ability = unitToAbility (unitTraits si) uid
    case target of
        TargetPos pos -> do
            agentModifyGrid (\grid -> addMark grid (getFootprint uid) pos)
            command [PointCommand ability [builder] (fromTuple pos)]
        TargetUnit targetUnit ->
            command [UnitCommand ability [builder] targetUnit]

unitHasOrder :: AbilityId -> Unit -> Bool
unitHasOrder order u = order `elem` orders
  where
    orders = toEnum' . view #abilityId <$> u ^. #orders

formatUnitOrders :: Unit -> String
formatUnitOrders unit = show orders
  where
    orders :: [AbilityId]
    orders = map (toEnum' . (^. #abilityId)) (unit ^. #orders)

traceBuilderOrders :: (HasObs d) => IntentId -> BuildStructurePhase -> StepMonad d ()
traceBuilderOrders iid phase =
    case phase of
        BSIssued _ builder _ _ -> logOrders builder
        BSAccepted builder _ _ -> logOrders builder
        BSMonitoring builder _ -> logOrders builder
        _ -> pure ()
  where
    logOrders builder = do
        obs <- agentObs
        case getUnit obs builder of
            Nothing -> traceM ("[builder-orders][" ++ show iid ++ "] builder missing: " ++ show builder)
            Just unit ->
                traceM
                    ( "[builder-orders]["
                        ++ show iid
                        ++ "] builder="
                        ++ show builder
                        ++ " orders="
                        ++ formatUnitOrders unit
                    )

targetInProgress :: (HasObs d) => Target -> UnitTypeId -> StepMonad d Bool
targetInProgress target uid = do
    obs <- agentObs
    pure $
        not . null $
            runC $
                unitsSelf obs
                    .| unitTypeC uid
                    .| filterC
                        ( \x ->
                            let dist = Spatial.distSquared (x ^. #pos) target
                             in trace ("[targetInProgress] unit pos: " ++ show (x ^. #pos) ++ ", target: " ++ show target ++ ", dist: " ++ show dist) (dist <= 1)
                        )

expectingPylons :: (HasObs d) => StepMonad d Bool
expectingPylons = do
    obs <- agentObs
    let pylonsInProgress =
            not . null $
                runC $
                    unitsSelf obs
                        .| unitTypeC ProtossPylon
                        .| filterC (\u -> u ^. #buildProgress < 1)
        pylonsOrdered =
            not . null $
                runC $
                    unitsSelf obs
                        .| unitTypeC ProtossProbe
                        .| filterC (unitHasOrder PROTOSSBUILDPYLON)
    pure (pylonsInProgress || pylonsOrdered)

buildPhaseTimedOut :: Word32 -> Word32 -> Bool
buildPhaseTimedOut now startedAt = now >= startedAt && now - startedAt > buildStartTimeoutFrames

finishFailure :: StepMonad d (IntentStepResult d)
finishFailure = stepDoneResult IntentFailed

liftBuildStepResult :: UnitTypeId -> BuildPhaseStepResult -> IntentStepResult d
liftBuildStepResult = liftLeafStepResult PBuildStructure

liftTrainStepResult :: UnitTypeId -> TrainPhaseStepResult -> IntentStepResult d
liftTrainStepResult = liftLeafStepResult PTrainUnit

liftLeafStepResult :: (outer -> leafPhase -> IntentProgram d) -> outer -> StepResult leafPhase -> IntentStepResult d
liftLeafStepResult wrap outer = fmap (wrap outer)

tickBuildPhase ::
    (HasObs d, HasGrid d, HasReservedCost d) =>
    IntentId ->
    UnitTypeId ->
    BuildStructurePhase ->
    StepMonad d BuildPhaseStepResult
tickBuildPhase iid uid currentPhase =
    let
        goNextFrame = stepBlockResult
        goNextTick = stepContinueResult
        retryNextFrame = goNextFrame currentPhase
        startMonitoring reserved builder target = do
            agentModifyReservedCost (+ reserved)
            goNextTick (BSMonitoring builder target)
     in
        case currentPhase of
            BSReserving -> do
                ucost <- agentUnitCost uid
                agentModifyReservedCost (\c -> c - ucost)
                goNextTick (BSWaitResources ucost)
            BSWaitResources reserved -> do
                canAfford <- agentCanAffordWith reserved uid
                canProduce <- abilityAvailableForUnit uid
                if not (canAfford && canProduce)
                    then retryNextFrame
                    else goNextTick (BSGathering reserved)
            BSGathering reserved -> do
                mbBuilder <- agentFindBuilder
                case mbBuilder of
                    Nothing -> retryNextFrame
                    Just builder -> do
                        placement <- findPlacementTarget uid
                        expectingPylonSupport <- expectingPylons
                        case placement of
                            Nothing ->
                                if expectingPylonSupport
                                    then retryNextFrame
                                    else stepNeedResult ProtossPylon
                            Just target -> do
                                commandBuild builder uid target
                                frame <- (^. #gameLoop) <$> agentObs
                                goNextFrame (BSIssued reserved (builder ^. #tag) target frame)
            BSIssued reserved builder target startFrame -> do
                obs <- agentObs
                si <- agentStatic
                frame <- (^. #gameLoop) <$> agentObs
                let ability = unitToAbility (unitTraits si) uid
                    mbBuilder = getUnit obs builder
                    timedOut = buildPhaseTimedOut frame startFrame
                started <- targetInProgress target uid
                let builderAccepted unit = unitHasOrder ability unit
                    advanceIssuedPhase
                        | started = startMonitoring reserved builder target
                        | isNothing mbBuilder = stepDoneResult IntentFailed
                        | maybe False builderAccepted mbBuilder = do
                            traceM
                                ( "[intent]["
                                    ++ show iid
                                    ++ "][build] command accepted; waiting for start"
                                    ++ " uid="
                                    ++ show uid
                                    ++ " builder="
                                    ++ show builder
                                    ++ " frame="
                                    ++ show frame
                                )
                            agentModifyReservedCost (+ reserved)
                            goNextFrame (BSAccepted builder target frame)
                        | timedOut = stepDoneResult IntentFailed
                        | otherwise = retryNextFrame
                advanceIssuedPhase
            BSAccepted builder target acceptedFrame -> do
                obs <- agentObs
                frame <- (^. #gameLoop) <$> agentObs
                let timedOut = buildPhaseTimedOut frame acceptedFrame
                    builderMissing = isNothing (getUnit obs builder)
                started <- targetInProgress target uid
                let advanceAcceptedPhase
                        | started = goNextTick (BSMonitoring builder target)
                        | builderMissing = stepDoneResult IntentFailed
                        | timedOut = stepDoneResult IntentFailed
                        | otherwise = retryNextFrame
                advanceAcceptedPhase
            BSMonitoring _ target -> do
                inProgress <- targetInProgress target uid
                if inProgress
                    then stepDoneResult IntentCompleted
                    else retryNextFrame

tickBuildStructure ::
    (HasObs d, HasGrid d, HasReservedCost d) =>
    IntentId ->
    UnitTypeId ->
    BuildStructurePhase ->
    StepMonad d (IntentStepResult d)
tickBuildStructure iid uid phase = liftBuildStepResult uid <$> tickBuildPhase iid uid phase

tickTrainUnit ::
    (HasObs d, HasReservedCost d) =>
    IntentId ->
    UnitTypeId ->
    TrainUnitPhase ->
    StepMonad d (IntentStepResult d)
tickTrainUnit iid uid phase = liftTrainStepResult uid <$> tickTrainPhase iid uid phase

tickTrainPhase ::
    (HasObs d, HasReservedCost d) =>
    IntentId ->
    UnitTypeId ->
    TrainUnitPhase ->
    StepMonad d TrainPhaseStepResult
tickTrainPhase _ uid currentPhase =
    let
        goNextFrame = stepBlockResult
        goNextTick = stepContinueResult
        retryNextFrame = goNextFrame currentPhase
     in
        case currentPhase of
            TUWaiting -> do
                canAfford <- agentCanAfford uid
                canProduce <- abilityAvailableForUnit uid
                if not (canAfford && canProduce)
                    then retryNextFrame
                    else do
                        ucost <- agentUnitCost uid
                        agentModifyReservedCost (\c -> c - ucost)
                        goNextTick (TUGathering ucost)
            TUGathering reserved -> do
                si <- agentStatic
                obs <- agentObs
                let ability = unitToAbility (unitTraits si) uid
                    producerType = abilityExecutor HashMap.! ability
                producer <- findProducerTag producerType
                case producer of
                    Nothing -> retryNextFrame
                    Just tag ->
                        case getUnit obs tag of
                            Nothing -> stepDoneResult IntentFailed
                            Just executor -> do
                                command [SelfCommand ability [executor]]
                                agentModifyReservedCost (+ reserved)
                                stepDoneResult IntentCompleted

tickProgram ::
    (HasObs d, HasGrid d, HasReservedCost d) =>
    IntentId ->
    IntentProgram d ->
    StepMonad d (IntentStepResult d)
tickProgram iid (PBuildStructure uid phase) = tickBuildStructure iid uid phase
tickProgram iid (PTrainUnit uid phase) = tickTrainUnit iid uid phase
tickProgram iid (PAndThen left right) = andThenStep right <$> tickProgram iid left
tickProgram iid (POrElse primary fallback) = orElseStep fallback <$> tickProgram iid primary

runIntent ::
    (HasObs d, HasGrid d, HasReservedCost d) =>
    IntentRuntime d ->
    StepMonad d (IntentRuntime d, IntentStatus)
runIntent = stepLoop maxIntentTransitionsPerFrame
  where
    stepLoop 0 rt = do
        traceM ("[intent][" ++ show (intentId rt) ++ "] transition budget exhausted for this frame")
        pure (rt, IntentRunning)
    stepLoop budget rt = do
        result <- intentTick rt
        case result of
            Continue rt' -> stepLoop (budget - 1) rt'
            Block rt' -> pure (rt', IntentRunning)
            Done rt' status -> pure (rt', status)
            NeedPrerequisite rt' uid -> pure (rt', IntentNeedsPrerequisite uid)

intentTick ::
    (HasObs d, HasGrid d, HasReservedCost d) =>
    IntentRuntime d ->
    StepMonad d (IntentTickResult d)
intentTick rt = do
    traceM ("[intent][" ++ show (intentId rt) ++ "] " ++ show (intentProgram rt))
    case intentProgram rt of
        PBuildStructure _ phase -> traceBuilderOrders (intentId rt) phase
        PTrainUnit _ _ -> pure ()
        PAndThen _ _ -> pure ()
        POrElse _ _ -> pure ()
    liftIntentStepResult rt <$> tickProgram (intentId rt) (intentProgram rt)

spawnIntentUnique :: (HasBuildIntents d, HasObs d) => IntentId -> IntentProgram d -> StepMonad d ()
spawnIntentUnique iid intent = do
    active <- intentExists iid
    traceM $ "[spawnIntentUnique] is_active:" <> show active
    if active then pure () else spawnIntent iid intent

intentEngine ::
    (HasObs d, HasGrid d, HasBuildIntents d, HasReservedCost d) =>
    StepMonad d IntentOutcomeStore
intentEngine = do
    obs <- agentObs
    frame <- pure (obs ^. #gameLoop)
    let resources = obsResources obs
    reserved <- agentGetReservedCost
    intents <- agentGet <&> (^. buildIntentsL) <&> Map.toList
    traceM $
        "==================== "
            <> show frame
            <> " ===================="
            <> " minerals: "
            <> show (mineralCost resources)
            <> " vespene: "
            <> show (gasCost resources)
            <> " reserved penalty: "
            <> show reserved
    traceM $ "[" <> show frame <> "][intentEngine] running intents: " <> show (length intents)
    outcomes <-
        mapM
            ( \(iid, i) -> do
                (i', status) <- runIntent i
                traceM $ "[" <> show frame <> "][intentEngine][" <> show iid <> "status: " <> show status
                case status of
                    IntentCompleted -> do
                        cleanupIntent (intentProgram i')
                        removeIntent iid
                        pure $ Just (iid, IntentCompleted)
                    IntentFailed -> do
                        cleanupIntent (intentProgram i')
                        removeIntent iid
                        pure $ Just (iid, IntentFailed)
                    IntentNeedsPrerequisite uid ->
                        updateIntent i'
                            >> spawnIntentUnique (IntentId ("prereq-" ++ show uid)) (buildStructureIntent uid)
                            >> pure Nothing
                    _ -> updateIntent i' >> pure Nothing
            )
            intents
    pure $ (Map.fromList $ catMaybes outcomes)

spawnIntent ::
    (HasObs d, HasBuildIntents d) =>
    IntentId ->
    IntentProgram d ->
    StepMonad d ()
spawnIntent iid program = do
    traceM $ "spawnIntent " ++ show iid
    frame <- (^. #gameLoop) <$> agentObs
    agentModify (buildIntentsL %~ Map.insert iid (IntentRuntime iid program frame))

intentExists :: (HasBuildIntents d) => IntentId -> StepMonad d Bool
intentExists iid = Map.member iid . (^. buildIntentsL) <$> agentGet

lookupIntent :: (HasBuildIntents d) => IntentId -> StepMonad d (Maybe (IntentRuntime d))
lookupIntent iid = Map.lookup iid . (^. buildIntentsL) <$> agentGet

updateIntent :: (HasBuildIntents d) => IntentRuntime d -> StepMonad d ()
updateIntent runtime = do
    traceM $ "updateIntent " ++ show (intentId runtime)
    agentModify (buildIntentsL %~ Map.insert (intentId runtime) runtime)

removeIntent :: (HasBuildIntents d) => IntentId -> StepMonad d ()
removeIntent iid = do
    traceM $ "removeIntent " ++ show iid
    agentModify (buildIntentsL %~ Map.delete iid)

stepIntent ::
    (HasObs d, HasGrid d, HasBuildIntents d, HasReservedCost d) =>
    IntentId ->
    StepMonad d IntentStatus
stepIntent iid = do
    current <- lookupIntent iid
    case current of
        Nothing -> pure IntentFailed
        Just runtime -> do
            (runtime', status) <- runIntent runtime
            traceM ("[intent][" ++ show iid ++ "] " ++ show status)
            case status of
                IntentRunning -> updateIntent runtime' >> pure IntentRunning
                IntentCompleted -> do
                    cleanupIntent (intentProgram runtime')
                    removeIntent iid
                    pure IntentCompleted
                IntentFailed -> do
                    cleanupIntent (intentProgram runtime')
                    removeIntent iid
                    pure IntentFailed
                IntentNeedsPrerequisite uid -> updateIntent runtime' >> pure (IntentNeedsPrerequisite uid)
