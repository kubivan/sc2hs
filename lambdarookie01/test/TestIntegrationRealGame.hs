{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestIntegrationRealGame (integrationRealGameTests) where

import Agent (Agent (..), StepPlan (..))
import BotDynamicState (BotDynamicState (..))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, catch)
import Data.HashMap.Strict qualified as HashMap
import Data.List (foldl', isInfixOf)
import Data.Map qualified as Map
import Data.ProtoLens.Labels ()
import Data.Text qualified as Text
import Lens.Micro ((&), (.~), (^.))
import Network.WebSockets qualified as WS
import Observation (Observation)
import PlanM (BuildOrder, boFromUnits)
import Proto.S2clientprotocol.Common qualified as C
import Proto.S2clientprotocol.Sc2api qualified as A
import SC2.Client (unitAbilities, unitAbilitiesRaw)
import SC2.Ids.UnitTypeId (UnitTypeId (ProtossAssimilator, ProtossCyberneticsCore, ProtossGateway, ProtossNexus, ProtossProbe, ProtossPylon, ProtossRoboticsFacility))
import SC2.Launcher.Participant (Participant (..))
import SC2.Proto.Data (Alliance (Self), Map (LocalMap))
import SC2.Proto.Requests qualified as Proto
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName)
import System.Process (readProcessWithExitCode)
import Test.Hspec
import TestBot (BotAgent (..), BotPhase (..))
import Units (toEnum')

integrationRealGameTests :: Spec
integrationRealGameTests =
    describe "Real game integration (opt-in)" $ do
        it "executes configured build order until buildings exist" $ do
            env <- integrationEnv
            case env of
                Nothing -> pendingWith "Set SC2_INTEGRATION=1 and SC2_TEST_MAP_PATH to run integration tests"
                Just cfg -> withGame cfg testBuildOrderCompletesBuildings

data IntegrationEnv = IntegrationEnv
    { ieMapPath :: FilePath
    , ieHost :: String
    , iePort :: Int
    , ieDockerImage :: String
    , ieContainerName :: String
    }

integrationEnv :: IO (Maybe IntegrationEnv)
integrationEnv = do
    enabled <- lookupEnv "SC2_INTEGRATION"
    case enabled of
        Just "1" -> do
            maybeMapPath <- lookupEnv "SC2_TEST_MAP_PATH"
            case maybeMapPath of
                Nothing -> pure Nothing
                Just mapPath -> do
                    mapExists <- doesFileExist mapPath
                    if not mapExists
                        then pure Nothing
                        else do
                            host <- lookupEnvDefault "SC2_HOST" "127.0.0.1"
                            port <- lookupEnvDefaultRead "SC2_PORT" 5555
                            dockerImage <- lookupEnvDefault "SC2_DOCKER_IMAGE" "ghcr.io/kubivan/aiurgaze-sc2:latest"
                            containerName <- lookupEnvDefault "SC2_CONTAINER_NAME" "sc2-int-test"
                            pure $
                                Just
                                    IntegrationEnv
                                        { ieMapPath = mapPath
                                        , ieHost = host
                                        , iePort = port
                                        , ieDockerImage = dockerImage
                                        , ieContainerName = containerName
                                        }
        _ -> pure Nothing

lookupEnvDefault :: String -> String -> IO String
lookupEnvDefault key fallback = maybe fallback id <$> lookupEnv key

lookupEnvDefaultRead :: Read a => String -> a -> IO a
lookupEnvDefaultRead key fallback = do
    value <- lookupEnv key
    pure $ maybe fallback read value

withGame :: IntegrationEnv -> (WS.Connection -> IO ()) -> IO ()
withGame cfg testAction =
    bracket
        (startContainer cfg)
        (\startMode -> case startMode of
            DockerStarted -> stopContainer (ieContainerName cfg)
            DockerReuseExisting -> pure ()
        )
        (\_ -> do
            waitForSc2 (ieHost cfg) (iePort cfg)
            WS.runClient (ieHost cfg) (iePort cfg) "/sc2api" testAction
        )

data DockerStart
    = DockerStarted
    | DockerReuseExisting

startContainer :: IntegrationEnv -> IO DockerStart
startContainer cfg = do
    let mapDir = takeDirectory (ieMapPath cfg)
    _ <- readProcessWithExitCode "docker" ["rm", "-f", ieContainerName cfg] ""
    runDockerChecked ["pull", ieDockerImage cfg]
    let mapsMount = mapDir <> ":/StarCraftII/maps"
        portMap = show (iePort cfg) <> ":" <> show (iePort cfg)
    (exitCode, _out, err) <-
        readProcessWithExitCode
            "docker"
            [ "run"
            , "-d"
            , "--rm"
            , "--name"
            , ieContainerName cfg
            , "-p"
            , portMap
            , "-v"
            , mapsMount
            , ieDockerImage cfg
            ]
            ""

    case exitCode of
        ExitSuccess -> pure DockerStarted
        ExitFailure _
            | "port is already allocated" `isInfixOf` err -> pure DockerReuseExisting
            | otherwise -> error $ "docker run failed: " <> err

stopContainer :: String -> IO ()
stopContainer containerName = do
    _ <- readProcessWithExitCode "docker" ["rm", "-f", containerName] ""
    pure ()

runDockerChecked :: [String] -> IO ()
runDockerChecked args = do
    (exitCode, out, err) <- readProcessWithExitCode "docker" args ""
    case exitCode of
        ExitSuccess -> pure ()
        ExitFailure code ->
            error $ "docker " <> unwords args <> " failed (" <> show code <> ")\nstdout:\n" <> out <> "\nstderr:\n" <> err

waitForSc2 :: String -> Int -> IO ()
waitForSc2 host port = loop (120 :: Int)
  where
    loop 0 = error "Timed out waiting for SC2 websocket"
    loop attemptsLeft = do
        result <-
            catch
                (WS.runClient host port "/sc2api" (\conn -> Proto.sendRequestSync conn Proto.requestPing >> pure True))
                (\(_ :: SomeException) -> pure False)
        if result
            then pure ()
            else threadDelay 1000000 >> loop (attemptsLeft - 1)

testBuildOrderCompletesBuildings :: WS.Connection -> IO ()
testBuildOrderCompletesBuildings conn = do
    agent0 <- initAgent conn
    responseObs <- Proto.sendRequestSync conn Proto.requestObservation
    let respObs = responseObs ^. #observation
    abilityRaw <- unitAbilitiesRaw conn respObs
    let abilityMap = unitAbilities abilityRaw
        obs = respObs ^. #observation
        fourGateBuild = [ProtossPylon, ProtossAssimilator, ProtossGateway, ProtossCyberneticsCore, ProtossAssimilator, ProtossGateway]
        expandBuild = [ProtossNexus, ProtossRoboticsFacility, ProtossGateway, ProtossGateway, ProtossAssimilator, ProtossAssimilator]
        targetBuildOrder = fourGateBuild ++ expandBuild

    injected <- case agent0 of
        EmptyBotAgent -> expectationFailure "agent unexpectedly became EmptyBotAgent" >> pure EmptyBotAgent
        BotAgent _ staticInfo ds env ->
            pure $ BotAgent (BuildOrderExecutor (boFromUnits targetBuildOrder) obs abilityMap) staticInfo ds{dsObs = obs} env

    waitUntilBuildOrderComplete conn 8000 injected

waitUntilBuildOrderComplete :: WS.Connection -> Int -> BotAgent -> IO ()
waitUntilBuildOrderComplete conn maxTicks startAgent = go 0 startAgent Nothing
  where
    requiredCounts =
        HashMap.fromList
            [ (ProtossPylon, 1)
            , (ProtossAssimilator, 4)
            , (ProtossGateway, 4)
            , (ProtossCyberneticsCore, 1)
            , (ProtossRoboticsFacility, 1)
            , (ProtossNexus, 2)
            ]

    interestingTypes =
        [ ProtossPylon
        , ProtossAssimilator
        , ProtossGateway
        , ProtossCyberneticsCore
        , ProtossRoboticsFacility
        , ProtossNexus
        ]

    phaseName :: BotPhase -> String
    phaseName Opening = "Opening"
    phaseName BuildOrderExecutor{} = "BuildOrderExecutor"
    phaseName BuildArmyAndWin{} = "BuildArmyAndWin"

    formatCounts :: HashMap.HashMap UnitTypeId Int -> String
    formatCounts counts =
        show
            [ (u, HashMap.lookupDefault 0 u counts)
            | u <- interestingTypes
            ]

    go n agent prevStatus
        | n >= maxTicks =
            expectationFailure $ "Timed out waiting for build-order completion after " <> show maxTicks <> " ticks"
        | otherwise = do
            (agent', _) <- runSingleTick conn agent
            case agent' of
                EmptyBotAgent -> expectationFailure "agent unexpectedly became EmptyBotAgent"
                BotAgent phase _ ds _ -> do
                    let obs = dsObs ds
                        counts = countCompletedSelfBuildings obs
                        allBuildingsPresent = allRequiredPresent requiredCounts counts
                        activeIntentCount = Map.size (dsIntents ds)
                        intentsCompleted = activeIntentCount == 0
                        queueCompleted =
                            case phase of
                                BuildArmyAndWin{} -> True
                                BuildOrderExecutor bo _ _ -> null bo
                                Opening -> False
                        queueLens =
                            case phase of
                                BuildOrderExecutor bo _ _ -> (length bo, activeIntentCount)
                                _ -> (0, activeIntentCount)
                        status =
                            ( phaseName phase
                            , fst queueLens
                            , snd queueLens
                            , allBuildingsPresent
                            )

                    if n `mod` 100 == 0 || Just status /= prevStatus
                        then
                            putStrLn $
                                "[build-order-progress] tick="
                                    <> show n
                                    <> " phase="
                                    <> phaseName phase
                                    <> " boLeft="
                                    <> show (fst queueLens)
                                    <> " activeIntents="
                                    <> show (snd queueLens)
                                    <> " allBuildingsPresent="
                                    <> show allBuildingsPresent
                                    <> " counts="
                                    <> formatCounts counts
                        else pure ()

                    if queueCompleted && intentsCompleted && allBuildingsPresent
                        then pure ()
                        else go (n + 1) agent' (Just status)

countCompletedSelfBuildings :: Observation -> HashMap.HashMap UnitTypeId Int
countCompletedSelfBuildings obs =
    foldl'
        (\acc u ->
            let unitType = toEnum' (u ^. #unitType) :: UnitTypeId
             in HashMap.insertWith (+) unitType 1 acc
        )
        HashMap.empty
        [ u
        | u <- obs ^. #rawData . #units
        , u ^. #alliance == Self
        , u ^. #buildProgress >= 1
        ]

allRequiredPresent :: HashMap.HashMap UnitTypeId Int -> HashMap.HashMap UnitTypeId Int -> Bool
allRequiredPresent required actual =
    all
        (\(unitType, needed) -> HashMap.lookupDefault 0 unitType actual >= needed)
        (HashMap.toList required)

initAgent :: WS.Connection -> IO BotAgent
initAgent conn = do
    mapPath <- requiredEnv "SC2_TEST_MAP_PATH"
    let mapFileName = takeFileName mapPath
        containerMapPath = "/StarCraftII/maps/" <> mapFileName
        hostPlayer = Player EmptyBotAgent
        aiPlayer = Computer C.Random A.VeryEasy Nothing
        createReq = Proto.requestCreateGame (LocalMap (Text.pack containerMapPath) Nothing) [hostPlayer, aiPlayer]
        joinReq = Proto.requestJoinGameVsAi C.Protoss

    _ <- Proto.sendRequestSync conn createReq
    responseJoin <- Proto.sendRequestSync conn joinReq
    responseGameInfo <- Proto.sendRequestSync conn Proto.requestGameInfo
    responseData <- Proto.sendRequestSync conn Proto.requestData
    responseObs0 <- Proto.sendRequestSync conn Proto.requestObservation

    let playerId = responseJoin ^. #joinGame . #playerId
        gi = responseGameInfo ^. #gameInfo
        gd = responseData ^. #data'
        obs0 = responseObs0 ^. #observation

    makeAgent EmptyBotAgent playerId gi gd obs0

runSingleTick :: WS.Connection -> BotAgent -> IO (BotAgent, StepPlan)
runSingleTick conn agent = do
    responseObs <- Proto.sendRequestSync conn Proto.requestObservation
    abilityRaw <- unitAbilitiesRaw conn (responseObs ^. #observation)
    let abilityMap = unitAbilities abilityRaw

    (agent', plan@(StepPlan cmds chats dbgs)) <- agentStep agent (responseObs ^. #observation) abilityMap
    _ <- Proto.sendRequestSync conn (Proto.requestAction cmds chats)
    _ <- Proto.sendRequestSync conn (Proto.requestDebug dbgs)
    _ <- Proto.sendRequestSync conn Proto.requestStep
    pure (agent', plan)

requiredEnv :: String -> IO String
requiredEnv key = do
    maybeValue <- lookupEnv key
    case maybeValue of
        Just value -> pure value
        Nothing -> error $ "Missing required env var: " <> key
