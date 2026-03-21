module Intent where

import Actions (Action (PointCommand, SelfCommand, UnitCommand), UnitTag)
import Conduit (filterC, mapC, (.|))
import Control.Monad (guard, unless)
import Control.Monad.Free (Free (..), liftF)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Function (on)
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HashMap
import Data.List (find, sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Word (Word32)
import Debug.Trace (trace, traceM)
import Footprint (getFootprint)
import Lens.Micro (Lens', (%~), (^.))
import Lens.Micro.Extras
import Observation (Cost (..), Observation, findNexus, getUnit, obsResources, obsUnitsC, unitsSelf)
import SC2.Geometry (fromTuple)
import SC2.Grid (Grid, TilePos, addMark, canPlaceBuilding, findPlacementPoint, findPlacementPointInRadius, tilePos)
import SC2.Ids.AbilityId
import SC2.Ids.UnitTypeId (UnitTypeId (ProtossAssimilator, ProtossNexus, ProtossProbe, ProtossPylon))
import SC2.Spatial qualified as Spatial
import SC2.TechTree (abilityExecutor, unitToAbility)
import StepMonad
import StepMonad (
    HasGrid,
    HasObs,
    HasReservedCost,
    MaybeStepMonad,
    StepMonad,
    agentAbilities,
    agentGet,
    agentGrid,
    agentModify,
    agentModifyGrid,
    agentObs,
    agentStatic,
    command,
    expandsPos,
    heightMap,
    unitTraits,
 )
import StepMonadUtils (abilityAvailableForUnit, agentCanAfford, agentCanAffordWith, agentUnitCost)
import Target (Target (..))
import Units (Unit, equalsC, fromEnum', isGeyser, mapTilePosC, runC, toEnum', unitTypeC)

newtype IntentId = IntentId [Char]
    deriving (Eq, Ord, Show)

data IntentStatus
    = IntentRunning
    | IntentCompleted
    | IntentFailed
    | IntentNeedsPrerequisite UnitTypeId
    deriving (Eq, Show)

data AwaitResult
    = AwaitContinue
    | AwaitBlock
    | AwaitFail
    | AwaitNeed UnitTypeId
    deriving (Eq, Show)

data IntentF d next
    = WaitUntil (IntentRuntime d -> StepMonad d Bool) next
    | Guard (IntentRuntime d -> StepMonad d Bool) next
    | AwaitWith (IntentRuntime d -> StepMonad d AwaitResult) next
    | ReserveCost UnitTypeId next
    | ReleaseCost UnitTypeId next
    | FindBuilder (UnitTag -> next)
    | FindPlacementTarget UnitTypeId (Target -> next)
    | FindProducerForUnit UnitTypeId (UnitTag -> next)
    | IssueBuild UnitTag UnitTypeId Target next
    | IssueSelfCommand UnitTag UnitTypeId next

instance Show (IntentF d next) where
    show (WaitUntil _ _) = "WaitUntil"
    show (Guard _ _) = "Guard"
    show (AwaitWith _ _) = "AwaitWith"
    show (ReserveCost _ _) = "ReserveCost"
    show (ReleaseCost _ _) = "ReleaseCost"
    show (FindBuilder _) = "FindBuilder"
    show (FindPlacementTarget _ _) = "FindPlacementTarget"
    show (FindProducerForUnit _ _) = "FindProducerForUnit"
    show (IssueBuild _ _ _ _) = "IssueBuild"
    show (IssueSelfCommand _ _ _) = "IssueSelfCommand"

instance Functor (IntentF d) where
    fmap f (WaitUntil cond next) = WaitUntil cond (f next)
    fmap f (Guard cond next) = Guard cond (f next)
    fmap f (AwaitWith cond next) = AwaitWith cond (f next)
    fmap f (ReserveCost uid next) = ReserveCost uid (f next)
    fmap f (ReleaseCost uid next) = ReleaseCost uid (f next)
    fmap f (FindBuilder k) = FindBuilder (f . k)
    fmap f (FindPlacementTarget uid k) = FindPlacementTarget uid (f . k)
    fmap f (FindProducerForUnit uid k) = FindProducerForUnit uid (f . k)
    fmap f (IssueBuild builder uid target next) = IssueBuild builder uid target (f next)
    fmap f (IssueSelfCommand producer uid next) = IssueSelfCommand producer uid (f next) -- fmap f (WaitForStart producer ability target next) = WaitForStart producer ability target (f next)

type IntentDSL d a = Free (IntentF d) a

data IntentTickResult d
    = Continue (IntentRuntime d)
    | Block (IntentRuntime d)
    | Done (IntentRuntime d) IntentStatus
    | NeedPrerequisite (IntentRuntime d) UnitTypeId

data IntentRuntime d = IntentRuntime
    { intentId :: IntentId
    , intentProgram :: IntentDSL d ()
    , intentStartedFrame :: Word32
    , intentReserve :: Cost
    }

type IntentStore d = Map IntentId (IntentRuntime d)

class HasBuildIntents d where
    buildIntentsL :: Lens' d (IntentStore d)

waitUntil :: StepMonad d Bool -> IntentDSL d ()
waitUntil cond = waitUntilWith (const cond)

waitUntilWith :: (IntentRuntime d -> StepMonad d Bool) -> IntentDSL d ()
waitUntilWith cond = liftF (WaitUntil cond ())

guardWithI :: (IntentRuntime d -> StepMonad d Bool) -> IntentDSL d ()
guardWithI cond = liftF (Guard cond ())

guardI :: StepMonad d Bool -> IntentDSL d ()
guardI cond = guardWithI (const cond)

awaitWith :: (IntentRuntime d -> StepMonad d AwaitResult) -> IntentDSL d ()
awaitWith cond = liftF (AwaitWith cond ())

reserveCostI :: UnitTypeId -> IntentDSL d ()
reserveCostI uid = liftF (ReserveCost uid ())

releaseCostI :: UnitTypeId -> IntentDSL d ()
releaseCostI uid = liftF (ReleaseCost uid ())

findBuilderI :: IntentDSL d UnitTag
findBuilderI = liftF (FindBuilder id)

findPlacementTargetI :: UnitTypeId -> IntentDSL d Target
findPlacementTargetI uid = liftF (FindPlacementTarget uid id)

findProducerForI :: UnitTypeId -> IntentDSL d UnitTag
findProducerForI uid = liftF (FindProducerForUnit uid id)

issueBuildI :: UnitTag -> UnitTypeId -> Target -> IntentDSL d ()
issueBuildI builder uid target = liftF (IssueBuild builder uid target ())

issueSelfCommandI :: UnitTag -> UnitTypeId -> IntentDSL d ()
issueSelfCommandI producer uid = liftF (IssueSelfCommand producer uid ())

releaseCostStep :: UnitTypeId -> StepMonad d ()
releaseCostStep _ = pure ()

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
findPlacementPos _ expands grid heightMap ProtossNexus = find (\pos -> canPlaceBuilding grid heightMap pos (getFootprint ProtossNexus)) expands
findPlacementPos obs _ grid heightMap ProtossPylon =
    findPlacementPoint grid heightMap (getFootprint ProtossPylon) nexusPos (const True)
  where
    nexusPos = tilePos $ findNexus obs ^. #pos
findPlacementPos obs _ grid heightMap uid = go pylons
  where
    go (p : ps) =
        case findPlacementPointInRadius grid heightMap (getFootprint uid) p 6.5 of
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

findPlacementTarget :: UnitTypeId -> Observation -> [TilePos] -> Grid -> Grid -> Maybe Target
findPlacementTarget uid obs expands grid gridHeight
    | uid == ProtossAssimilator = TargetUnit <$> findFreeGeyser obs
    | otherwise = TargetPos <$> findPlacementPos obs expands grid gridHeight uid

spawnIntentUnique iid intent = do
    active <- intentExists iid
    traceM $ "[spawnIntentUnique] is_active:" <> (show active)
    unless active $ spawnIntent iid intent

intentEngine :: (HasObs d, HasGrid d, HasBuildIntents d, HasReservedCost d) => StepMonad d ()
intentEngine = do
    frame <- agentObs <&> (^. #gameLoop)
    intents <- agentGet <&> (^. buildIntentsL) <&> Map.toList
    mapM_
        ( \(iid, i) -> do
            (i', status) <- runIntent i
            traceM $ "[" <> show frame <> "][intentEngine][" <> (show iid) <> "status: " <> (show status)
            case status of
                IntentCompleted -> removeIntent iid
                IntentFailed -> removeIntent iid
                IntentNeedsPrerequisite uid -> updateIntent i' >> spawnIntentUnique (IntentId ("prereq-" ++ show uid)) (ensureStructure uid)
                _ -> updateIntent i'
        )
        intents

runIntent ::
    (HasObs d, HasGrid d, HasReservedCost d) =>
    IntentRuntime d ->
    StepMonad d (IntentRuntime d, IntentStatus)
runIntent = stepLoop
  where
    stepLoop rt = do
        result <- intentTick rt
        case result of
            Continue rt' -> stepLoop rt'
            Block rt' -> pure (rt', IntentRunning)
            Done rt' status -> pure (rt', status)
            NeedPrerequisite rt' uid -> pure (rt', IntentNeedsPrerequisite uid)

intentTick :: (HasObs d, HasGrid d, HasReservedCost d) => IntentRuntime d -> StepMonad d (IntentTickResult d)
intentTick rt =
    case intentProgram rt of
        Pure _ -> pure $ Done rt IntentCompleted
        Free instruction -> do
            traceM ("[intent][" ++ show (intentId rt) ++ "] " ++ show instruction)
            case instruction of
                WaitUntil cond next -> do
                    ready <- cond rt
                    if ready
                        then pure $ Continue (rt{intentProgram = next})
                        else pure $ Block rt
                Guard cond next -> do
                    ok <- cond rt
                    if ok
                        then pure $ Continue (rt{intentProgram = next})
                        else pure $ Done rt IntentFailed
                AwaitWith cond next -> do
                    result <- cond rt
                    case result of
                        AwaitContinue -> pure $ Continue (rt{intentProgram = next})
                        AwaitBlock -> pure $ Block rt
                        AwaitFail -> pure $ Done rt IntentFailed
                        AwaitNeed uid -> pure $ NeedPrerequisite rt uid
                ReserveCost uid next -> do
                    ucost <- agentUnitCost uid
                    -- subtract from the global reserve, add to the local
                    _ <- agentModifyReservedCost (\c -> c - ucost)
                    pure $ Continue (rt{intentProgram = next, intentReserve = (intentReserve rt) + ucost})
                ReleaseCost uid next -> do
                    ucost <- agentUnitCost uid
                    agentModifyReservedCost (+ ucost)
                    pure $ Continue (rt{intentProgram = next, intentReserve = (intentReserve rt) - ucost})
                FindBuilder k -> do
                    mbBuilder <- agentFindBuilder
                    case mbBuilder of
                        Nothing -> pure $ Block rt
                        Just builder -> pure $ Continue (rt{intentProgram = k (builder ^. #tag)}) -- (IntentRuntime iid (k (builder ^. #tag)) startedFrame, IntentRunning)
                FindPlacementTarget uid k -> do
                    obs <- agentObs
                    grid <- agentGrid
                    si <- agentStatic
                    case findPlacementTarget uid obs (expandsPos si) grid (heightMap si) of
                        Nothing -> pure $ Block rt
                        -- IntentRuntime iid (k target) startedFrame, IntentRunning)
                        Just target -> pure $ Continue (rt{intentProgram = k target})
                FindProducerForUnit uid k -> do
                    si <- agentStatic
                    let ability = unitToAbility (unitTraits si) uid
                        producerType = abilityExecutor HashMap.! ability
                    producer <- findProducerTag producerType
                    case producer of
                        Nothing -> pure $ Block rt
                        Just tag -> pure $ Continue (rt{intentProgram = k tag})
                IssueBuild builder uid target next -> do
                    si <- agentStatic
                    obs <- agentObs
                    let ability = unitToAbility (unitTraits si) uid
                    case find ((== builder) . (^. #tag)) (obs ^. #rawData . #units) of
                        Nothing -> pure $ Done rt IntentFailed
                        Just executor -> do
                            case target of
                                TargetPos pos -> do
                                    agentModifyGrid (\grid -> addMark grid (getFootprint uid) pos)
                                    command [PointCommand ability [executor] (fromTuple pos)]
                                    pure $ Continue (rt{intentProgram = next})
                                TargetUnit targetUnit -> do
                                    command [UnitCommand ability [executor] targetUnit]
                                    pure $ Continue (rt{intentProgram = next})
                IssueSelfCommand producer uid next -> do
                    si <- agentStatic
                    obs <- agentObs
                    let ability = unitToAbility (unitTraits si) uid
                    case find ((== producer) . (^. #tag)) (obs ^. #rawData . #units) of
                        Nothing -> pure $ Done rt IntentFailed
                        Just executor -> do
                            command [SelfCommand ability [executor]]
                            pure $ Continue (rt{intentProgram = next})

spawnIntent :: (HasObs d, HasBuildIntents d, HasReservedCost d) => IntentId -> IntentDSL d () -> StepMonad d ()
spawnIntent iid program = do
    traceM $ "spawnIntent " ++ (show iid)
    frame <- (^. #gameLoop) <$> agentObs
    agentModify (buildIntentsL %~ Map.insert iid (IntentRuntime iid program frame (Cost 0 0)))

intentExists :: (HasBuildIntents d) => IntentId -> StepMonad d Bool
intentExists iid = Map.member iid . (^. buildIntentsL) <$> agentGet

lookupIntent :: (HasBuildIntents d) => IntentId -> StepMonad d (Maybe (IntentRuntime d))
lookupIntent iid = Map.lookup iid . (^. buildIntentsL) <$> agentGet

updateIntent :: (HasBuildIntents d) => IntentRuntime d -> StepMonad d ()
updateIntent runtime = do
    traceM $ "updateIntent " ++ (show $ intentId runtime)
    -- unless (runtime == ) $
    agentModify (buildIntentsL %~ Map.insert (intentId runtime) runtime)

removeIntent :: (HasBuildIntents d) => IntentId -> StepMonad d ()
removeIntent iid = do
    traceM $ "removeIntent " ++ (show iid)
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
                IntentCompleted -> removeIntent iid >> pure IntentCompleted
                IntentFailed -> do
                    traceM $ "stepIntent: intent failed"
                    agentModifyReservedCost (+ intentReserve runtime')
                    removeIntent iid
                    pure IntentFailed
                IntentNeedsPrerequisite uid -> updateIntent runtime' >> pure (IntentNeedsPrerequisite uid)

-- TODO: duplicate
unitHasOrder :: AbilityId -> Units.Unit -> Bool
unitHasOrder order u = order `elem` orders
  where
    orders = toEnum' . view #abilityId <$> u ^. #orders

canAffordI uid rt = do
    obs <- agentObs
    reserved <- agentGetReservedCost
    traceM
        ( "[intent]["
            ++ show (intentId rt)
            ++ "] "
            ++ " can i afford "
            ++ show uid
            ++ " intent reserve: "
            ++ (show $ intentReserve rt)
            ++ " global reserve: "
            ++ show (reserved, obsResources obs)
        )
    agentCanAffordWith (intentReserve rt) uid

canProduceI uid rt = do
    traceM ("[intent][" ++ show (intentId rt) ++ "] " ++ " can i produce " ++ show uid)
    abilityAvailableForUnit uid

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

ensureStructure :: (HasObs d, HasReservedCost d, HasGrid d) => UnitTypeId -> IntentDSL d ()
ensureStructure uid = do
    reserveCostI uid
    waitUntilWith (canAffordI uid)
    waitUntilWith (canProduceI uid)
    builder <- findBuilderI
    target <- ensurePlacement uid
    issueBuildI builder uid target
    awaitWith $ \rt -> do
        obs <- agentObs
        si <- agentStatic
        let ability = unitToAbility (unitTraits si) uid
            builderHasOrder =
                case getUnit obs builder of
                    Just b -> unitHasOrder ability b
                    Nothing -> False

            targetInProgress =
                -- not . null $
                runC $
                    unitsSelf obs
                        -- .| unitTypeC uid
                        .| filterC (\x -> (toEnum' $ x ^. #unitType) /= ProtossProbe)
                        .| mapC (\x -> (toEnum' $ x ^. #unitType :: UnitTypeId, Spatial.distSquared target x))
        -- .| filterC
        --     ( \x ->
        --         let dist = Spatial.distSquared (x ^. #pos) target
        --          in trace ("[ensureStructure] unit pos: " ++ show (x ^. #pos) ++ ", target: " ++ show target ++ ", dist: " ++ show dist) (dist <= 2 * 2)
        --     )

        traceM ("[intent][" ++ show (intentId rt) ++ "] waiting for build start " ++ show uid ++ " " ++ show targetInProgress)

        if builderHasOrder
            then
                traceM "order in progress, wait" >> pure AwaitBlock
            else
                traceM "order failed or completed, check next frame " >> pure AwaitContinue

    releaseCostI uid -- release cost, we either done, or failed
    waitUntil (targetInProgress target uid)

ensureUnit :: (HasObs d, HasReservedCost d) => UnitTypeId -> IntentDSL d ()
ensureUnit uid = do
    waitUntilWith (\rt -> agentCanAffordWith (intentReserve rt) uid)
    waitUntil (abilityAvailableForUnit uid)
    reserveCostI uid
    producer <- findProducerForI uid
    issueSelfCommandI producer uid
    releaseCostI uid

ensurePlacement ::
    (HasObs d, HasGrid d) =>
    UnitTypeId -> IntentDSL d Target
ensurePlacement uid = do
    awaitWith $ \_ -> do
        obs <- agentObs
        grid <- agentGrid
        si <- agentStatic

        let placement =
                findPlacementTarget uid obs (expandsPos si) grid (heightMap si)

            pylonsInProgress =
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

        case placement of
            Just _ -> pure AwaitContinue
            Nothing ->
                if pylonsInProgress || pylonsOrdered
                    then
                        pure AwaitBlock
                    else
                        traceM ("[intent][ensurePlacement] no pylons in progress, no placement point, we need one more pylon")
                            >> pure (AwaitNeed ProtossPylon)

    findPlacementTargetI uid
