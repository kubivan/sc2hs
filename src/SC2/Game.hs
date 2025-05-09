{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SC2.Game (playMatch) where

import Agent

-- import SC2.Proto (Participant, requestJoinGame1vs1, requestJoinGameVsAi, Race, Request)

import SC2.Client (
    GameSignals,
    newGameSignals,
    signalClientJoined,
    signalGameCreated,
    startStarCraft,
    unitAbilities,
    unitAbilitiesRaw,
    waitAllClientsJoined,
    waitForGameCreation,
 )
import SC2.Config
import SC2.Grid.Core (gridFromImage)
import SC2.Grid.Utils (printGrid)
import SC2.Participant
import SC2.Proto.Data
import SC2.Proto.Data qualified as Proto
import SC2.Proto.Requests
import SC2.Proto.Requests qualified as Proto

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (async, wait)
import Control.Monad.Writer.Strict (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.Functor ((<&>))
import Data.ProtoLens.Encoding (encodeMessage)
import Data.ProtoLens.Labels ()
import Debug.Trace (traceM)
import GHC.Word (Word32)
import Lens.Micro ((^.))
import Network.WebSockets as WS (
    Connection,
    runClient,
 )

playMatch :: Participant -> Participant -> IO ()
-- 1vs1
playMatch firstParticipant secondParticipant@(Player{}) = do
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

clientAppJoinGame :: (Agent a) => Connection -> a -> GameSignals -> (Race -> Request) -> IO [PlayerResult]
clientAppJoinGame conn agent signals joinFunc = do
    responseJoinGame <- Proto.sendRequestSync conn $ joinFunc (Agent.agentRace agent)
    print responseJoinGame
    let playerId = responseJoinGame ^. #joinGame . #playerId
    traceM $ "playerId " ++ show playerId
    signalClientJoined signals

    runGameLoop conn signals agent playerId

playHost :: Participant -> [Participant] -> GameSignals -> (Race -> Request) -> IO [PlayerResult]
playHost (Computer _) _ _ _ = Prelude.error "computer cannot be the host"
playHost (Player agent) participants signals joinFunc = do
    traceM "starting sc2 host"
    _ <- startStarCraft portHost
    traceM "running host"
    WS.runClient hostName (fromIntegral portHost) "/sc2api" clientApp
  where
    clientApp conn = putStrLn "creating game..." >> clientAppCreateGame conn participants signals >> putStrLn "Host joining game..." >> clientAppJoinGame conn agent signals joinFunc

playClient :: Participant -> GameSignals -> (Race -> Request) -> IO [PlayerResult]
playClient (Computer _) _ _ = Prelude.error "Computer cannot be host"
playClient (Player agent) signals joinFunc = do
    traceM "starting sc2 for second player"
    threadId <- myThreadId
    putStrLn $ "Current thread ID: " ++ show threadId
    _ <- startStarCraft portClient
    waitForGameCreation signals
    WS.runClient hostName (fromIntegral portClient) "/sc2api" clientApp
  where
    clientApp conn = putStrLn "client joining game..." >> clientAppJoinGame conn agent signals joinFunc

runGameLoop :: (Agent a) => Connection -> GameSignals -> a -> Word32 -> IO [PlayerResult]
runGameLoop conn signals agent playerId = do
    putStrLn $ "Player" ++ show playerId ++ "waiting for host to create the game..."
    waitAllClientsJoined signals

    putStrLn "getting game info..."
    respGameInfo <- Proto.sendRequestSync conn Proto.requestGameInfo
    gameDataResp <- Proto.sendRequestSync conn Proto.requestData
    let gi :: Proto.ResponseGameInfo = respGameInfo ^. #gameInfo
        gd :: Proto.ResponseData = gameDataResp ^. #data'
        -- Just gd = gameDataResp ^. #maybe'data
        pathingGrid = gridFromImage (gi ^. #startRaw . #pathingGrid)

    printGrid pathingGrid
    liftIO $ B.writeFile "grids/gameinfo" (encodeMessage gi)

    obs0 <- Proto.sendRequestSync conn Proto.requestObservation

    agent' <- makeAgent agent playerId gi gd (obs0 ^. #observation)

    gameStepLoop conn agent'

gameStepLoop :: (Agent agent) => Connection -> agent -> IO [PlayerResult]
gameStepLoop conn agent = do
    responseObs <- Proto.sendRequestSync conn Proto.requestObservation
    let gameOver = responseObs ^. #observation . #playerResult

    if not (null gameOver)
        then return gameOver
        else do
            let obs = responseObs ^. #observation . #observation
                gameLoop = obs ^. #gameLoop

            abilities <- unitAbilitiesRaw conn obs <&> unitAbilities
            (agent', StepPlan cmds chats dbgs) <- return $ Agent.agentStep agent (responseObs ^. #observation) abilities -- UnitAbilities

            -- (agent', StepPlan cmds chats dbgs, ds') <- return $ runStep si abilities (setObs obs ds) (Agent.agentStep agent)
            -- liftIO $ gridToFile ("grids/grid" ++ show gameLoop ++ ".txt") (getGrid ds')
            -- liftIO $ B.writeFile ("grids/obs" ++ show gameLoop) (encodeMessage obs)
            -- Prelude.putStrLn $ show gameLoop ++ " buildOrder " ++ show bo ++ " queue: " ++ show q
            _ <- liftIO . Proto.sendRequestSync conn $ Proto.requestAction cmds chats
            _ <- liftIO . Proto.sendRequestSync conn . Proto.requestDebug $ dbgs
            _ <- liftIO . Proto.sendRequestSync conn $ Proto.requestStep
            gameStepLoop conn agent'