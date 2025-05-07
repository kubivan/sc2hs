{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SC2.Game (playMatch, Proto.Participant (..)) where

import Agent (
    Agent (agentRace, agentStep, makeDynamicState),
    AgentDynamicState (setObs),
    StaticInfo (StaticInfo, siRegions),
    StepPlan (StepPlan),
    runStep,
 )
import Observation (enemyBaseLocation, findExpands, unitsSelf)
import Proto (Participant, requestJoinGame1vs1, requestJoinGameVsAi)
import Proto qualified
import SC2.Client (
    GameSignals,
    newGameSignals,
    signalClientJoined,
    signalGameCreated,
    startStarCraft,
    unitAbilities,
    unitAbilitiesRaw,
    unitsData,
    waitAllClientsJoined,
    waitForGameCreation,
 )
import SC2.Config
import UnitTypeId
import Units (runC, unitTypeC)
import Utils (distSquared, tilePos)

import Conduit ((.|))
import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (async, wait)
import Control.Monad.Writer.Strict (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.ProtoLens.Encoding (encodeMessage)
import Data.ProtoLens.Labels ()
import Debug.Trace (trace, traceM)
import GHC.Word (Word32)
import Grid.Algo (
    buildRegionGraph,
    buildRegionLookup,
    findAllChokePoints,
    gridSegment,
 )
import Grid.Core (gridFromImage, gridPixel)
import Grid.Utils (gridMerge, pixelIsRamp, printGrid)
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)
import Network.WebSockets as WS (
    Connection,
    runClient,
 )
import Proto.S2clientprotocol.Common qualified as A
import Proto.S2clientprotocol.Sc2api as S (
    PlayerResult,
    Request,
    ResponseGameInfo,
 )

playMatch :: Proto.Participant -> Proto.Participant -> IO ()
-- 1vs1
playMatch firstParticipant secondParticipant@(Proto.Player{}) = do
    let joinRequest = requestJoinGame1vs1 serverPortSet clientPortSet
    signals <- newGameSignals 2

    asyncHostTask <- async $ do
        threadId <- myThreadId
        putStrLn $ "Host thread ID: " ++ show threadId
        playHost firstParticipant [firstParticipant, secondParticipant] signals joinRequest

    asyncClientTask <- async $ playClient secondParticipant signals joinRequest

    gameOver <- wait asyncHostTask
    _ <- wait asyncClientTask
    putStrLn $ "Game Ended: " ++ show gameOver

-- 1vsComputer
playMatch firstParticipant secondParticipant = do
    let (joinRequest, playersCount) = (requestJoinGameVsAi, 1)

    signals <- newGameSignals playersCount
    asyncHostTask <- async $ do
        threadId <- myThreadId
        putStrLn $ "Host thread ID: " ++ show threadId
        playHost firstParticipant [firstParticipant, secondParticipant] signals joinRequest

    gameOver <- wait asyncHostTask
    putStrLn $ "Game Ended: " ++ show gameOver

clientAppCreateGame :: Connection -> [Participant] -> GameSignals -> IO ()
clientAppCreateGame conn participants signals = do
    putStrLn "creating game..."
    responseCreateGame <- Proto.sendRequestSync conn $ Proto.requestCreateGame (Proto.LocalMap "ai/2000AtmospheresAIE.SC2Map" Nothing) participants
    print responseCreateGame
    signalGameCreated signals

clientAppJoinGame :: Agent a d => Connection -> a -> GameSignals -> (A.Race -> Request) -> IO [PlayerResult]
clientAppJoinGame conn agent signals joinFunc = do
    responseJoinGame <- Proto.sendRequestSync conn $ joinFunc (Agent.agentRace agent)
    print responseJoinGame
    let playerId = responseJoinGame ^. #joinGame . #playerId
    traceM $ "playerId " ++ show playerId
    signalClientJoined signals

    runGameLoop conn signals agent playerId

playHost :: Proto.Participant -> [Proto.Participant] -> GameSignals -> (A.Race -> S.Request) -> IO [PlayerResult]
playHost (Proto.Computer _) _ _ _ = Prelude.error "computer cannot be the host"
playHost (Proto.Player agent) participants signals joinFunc = do
    traceM "starting sc2 host"
    _ <- startStarCraft portHost
    traceM "running host"
    WS.runClient hostName (fromIntegral portHost) "/sc2api" clientApp
  where
    clientApp conn = putStrLn "creating game..." >> clientAppCreateGame conn participants signals >> putStrLn "Host joining game..." >> clientAppJoinGame conn agent signals joinFunc

playClient :: Proto.Participant -> GameSignals -> (A.Race -> Request) -> IO [PlayerResult]
playClient (Proto.Computer _) _ _ = Prelude.error "Computer cannot be host"
playClient (Proto.Player agent) signals joinFunc = do
    traceM "starting sc2 for second player"
    threadId <- myThreadId
    putStrLn $ "Current thread ID: " ++ show threadId
    _ <- startStarCraft portClient
    waitForGameCreation signals
    WS.runClient hostName (fromIntegral portClient) "/sc2api" clientApp
  where
    clientApp conn = putStrLn "client joining game..." >> clientAppJoinGame conn agent signals joinFunc

runGameLoop :: (Agent a d) => Connection -> GameSignals -> a -> Word32 -> IO [PlayerResult]
runGameLoop conn signals agent playerId = do
    putStrLn $ "Player" ++ show playerId ++ "waiting for host to create the game..."
    waitAllClientsJoined signals

    putStrLn "getting game info..."
    resp <- Proto.sendRequestSync conn Proto.requestGameInfo
    let gi :: S.ResponseGameInfo = resp ^. #gameInfo
        playerInfos = gi ^. #playerInfo
        playerGameInfo = head $ filter (\gi -> gi ^. #playerId == playerId) playerInfos
        pathingGrid = gridFromImage (gi ^. #startRaw . #pathingGrid)

    printGrid pathingGrid
    liftIO $ B.writeFile "grids/gameinfo" (encodeMessage gi)

    gameDataResp <- Proto.sendRequestSync conn Proto.requestData
    obs0 <- Proto.sendRequestSync conn Proto.requestObservation

    let heightMap = gridFromImage $ gi ^. #startRaw . #terrainHeight
        obsRaw = obs0 ^. #observation . #observation
        unitTraits = unitsData $ gameDataResp ^. #data' . #units
        gridPlacement = gridFromImage $ gi ^. #startRaw . #placementGrid
        gridPathing = gridFromImage $ gi ^. #startRaw . #pathingGrid
        grid = gridMerge pixelIsRamp gridPlacement gridPathing
        nexusPos = view #pos $ head $ runC $ unitsSelf obsRaw .| unitTypeC ProtossNexus
        -- TODO: sort expands based on region connectivity: closest is not always the next one
        expands = sortOn (distSquared nexusPos) $ findExpands obsRaw grid heightMap
        enemyStart = tilePos $ enemyBaseLocation gi obsRaw

        (rays, gridChoked) = findAllChokePoints gridPathing
        regions = gridSegment gridChoked
        regionGraph = buildRegionGraph regions
        regionLookup = buildRegionLookup regions
        si = Agent.StaticInfo gi playerGameInfo unitTraits heightMap expands enemyStart regionGraph regionLookup (Map.fromList regions)

    traceM "create ds"
    dynamicState <- makeDynamicState agent obsRaw grid
    traceM "created ds"

    gameStepLoop conn si dynamicState agent

gameStepLoop :: (Agent agent d) => Connection -> Agent.StaticInfo -> d -> agent -> IO [S.PlayerResult]
gameStepLoop conn si ds agent = do
    responseObs <- Proto.sendRequestSync conn Proto.requestObservation
    let gameOver = responseObs ^. #observation . #playerResult

    if not (null gameOver)
        then return gameOver
        else do
            let obs = responseObs ^. #observation . #observation
                gameLoop = obs ^. #gameLoop

            abilities <- unitAbilitiesRaw conn obs <&> unitAbilities
            (agent', StepPlan cmds chats dbgs, ds') <- return $ runStep si abilities (setObs obs ds) (Agent.agentStep agent)
            -- liftIO $ gridToFile ("grids/grid" ++ show gameLoop ++ ".txt") (getGrid ds')
            -- liftIO $ B.writeFile ("grids/obs" ++ show gameLoop) (encodeMessage obs)
            -- Prelude.putStrLn $ show gameLoop ++ " buildOrder " ++ show bo ++ " queue: " ++ show q
            _ <- liftIO . Proto.sendRequestSync conn $ Proto.requestAction cmds chats
            _ <- liftIO . Proto.sendRequestSync conn . Proto.requestDebug $ dbgs
            _ <- liftIO . Proto.sendRequestSync conn $ Proto.requestStep
            gameStepLoop conn si ds' agent'