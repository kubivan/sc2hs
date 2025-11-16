{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SC2.Game (
    NetworkSettings (..),
    HostRuntime (..),
    JoinRuntime (..),
    playMatch,
    joinMatch,
) where

import Agent

-- import SC2.Proto (Participant, requestJoinGame1vs1, requestJoinGameVsAi, Race, Request)

import SC2.BotConfig (StarCraft2Config (..))
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
import SC2.Grid.Core (gridFromImage)
import SC2.Participant
import SC2.Proto.Data (Map, PlayerResult, Race)
import SC2.Proto.Data qualified as Proto
import SC2.Proto.Requests
import SC2.Proto.Requests qualified as Proto

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (async, wait)
import Control.Monad (when)
import Control.Monad.Writer.Strict (MonadIO (liftIO))
import Data.ByteString qualified as B
import Data.Functor ((<&>))
import Data.Int (Int32)
import Data.ProtoLens.Encoding (encodeMessage)
import Data.ProtoLens.Labels ()
import Debug.Trace (traceM)
import GHC.Word (Word32)
import Lens.Micro ((^.))
import Network.WebSockets as WS (
    Connection,
    runClient,
 )
import System.Directory(createDirectoryIfMissing)


data NetworkSettings = NetworkSettings
    { networkHostName :: String
    , networkHostPort :: Int
    , networkClientPort :: Int
    }
    deriving (Show)

data HostRuntime = HostRuntime
    { hostParticipant :: Participant
    , opponentParticipant :: Participant
    , hostNetwork :: NetworkSettings
    , hostStarcraft :: StarCraft2Config
    , hostMap :: Map
    }

data JoinRuntime = JoinRuntime
    { joinParticipant :: Participant
    , joinNetwork :: NetworkSettings
    }

serverPortSet :: NetworkSettings -> (Int32, Int32)
serverPortSet NetworkSettings{networkHostPort = hostBasePort} =
    ( fromIntegral (hostBasePort + 2)
    , fromIntegral (hostBasePort + 3)
    )

clientPortSet :: NetworkSettings -> (Int32, Int32)
clientPortSet NetworkSettings{networkHostPort = hostBasePort} =
    ( fromIntegral (hostBasePort + 4)
    , fromIntegral (hostBasePort + 5)
    )

playMatch :: HostRuntime -> IO ()
playMatch (HostRuntime hostPart opponentPart net sc2Cfg mapSpec) =
    case opponentPart of
        Computer{} -> hostVsComputer
        Player{} -> hostVsPlayer
  where
    participants = [hostPart, opponentPart]

    hostVsComputer = do
        let joinRequest = requestJoinGameVsAi
        signals <- newGameSignals 1
        results <- runHostSession sc2Cfg mapSpec net hostPart participants signals joinRequest
        putStrLn $ "Game Ended: " ++ show results

    hostVsPlayer = do
        let joinRequest = requestJoinGame1vs1 (serverPortSet net) (clientPortSet net)
        signals <- newGameSignals 2
        asyncHostTask <- async $ runHostSession sc2Cfg mapSpec net hostPart participants signals joinRequest
        asyncClientTask <- async $ runClientSession net opponentPart signals joinRequest
        hostResults <- wait asyncHostTask
        _ <- wait asyncClientTask
        putStrLn $ "Game Ended: " ++ show hostResults

joinMatch :: JoinRuntime -> IO ()
joinMatch (JoinRuntime participant net) =
    case participant of
        Computer{} -> Prelude.error "computer cannot join as a player"
        Player agent -> do
            signals <- newGameSignals 1
            let joinRequest = requestJoinGame1vs1Created
            WS.runClient (networkHostName net) (networkHostPort net) "/sc2api" $ \conn -> do
                putStrLn "client joining game..."
                _ <- clientAppJoinGame conn agent signals joinRequest
                pure ()

clientAppCreateGame :: Connection -> Map -> [Participant] -> GameSignals -> IO ()
clientAppCreateGame conn mapSpec participants signals = do
    putStrLn "creating game..."
    responseCreateGame <- Proto.sendRequestSync conn $ Proto.requestCreateGame mapSpec participants
    print responseCreateGame
    signalGameCreated signals

clientAppJoinGame :: (Agent a) => Connection -> a -> GameSignals -> (Race -> Proto.Request) -> IO [PlayerResult]
clientAppJoinGame conn agent signals joinFunc = do
    responseJoinGame <- Proto.sendRequestSync conn $ joinFunc (Agent.agentRace agent)
    print $ "responseJoinGame: " ++ (show responseJoinGame)
    let joinedPlayerId = responseJoinGame ^. #joinGame . #playerId
    traceM $ "playerId " ++ show joinedPlayerId
    signalClientJoined signals

    runGameLoop conn signals agent joinedPlayerId

runHostSession :: StarCraft2Config -> Map -> NetworkSettings -> Participant -> [Participant] -> GameSignals -> (Race -> Proto.Request) -> IO [PlayerResult]
runHostSession _ _ _ (Computer _ _ _) _ _ _ = Prelude.error "computer cannot be the host"
runHostSession sc2Cfg mapSpec net (Player agent) participants signals joinFunc = do
        when (start sc2Cfg) $
                startStarCraft sc2Cfg (networkHostName net) (fromIntegral $ networkHostPort net)
        threadId <- myThreadId
        putStrLn $ "Host thread ID: " ++ show threadId
        WS.runClient (networkHostName net) (networkHostPort net) "/sc2api" clientApp
    where
        clientApp conn = do
                putStrLn "creating game..."
                clientAppCreateGame conn mapSpec participants signals
                putStrLn "Host joining game..."
                clientAppJoinGame conn agent signals joinFunc

runClientSession :: NetworkSettings -> Participant -> GameSignals -> (Race -> Proto.Request) -> IO [PlayerResult]
runClientSession _ (Computer _ _ _) _ _ = Prelude.error "computer cannot be the client"
runClientSession net (Player agent) signals joinFunc = do
        threadId <- myThreadId
        putStrLn $ "Client thread ID: " ++ show threadId
        waitForGameCreation signals
        WS.runClient (networkHostName net) (networkClientPort net) "/sc2api" clientApp
    where
        clientApp conn = do
                putStrLn "client joining game..."
                clientAppJoinGame conn agent signals joinFunc

runGameLoop :: (Agent a) => Connection -> GameSignals -> a -> Word32 -> IO [PlayerResult]
runGameLoop conn signals agent localPlayerId = do
    putStrLn $ "Player" ++ show localPlayerId ++ "waiting for host to create the game..."
    waitAllClientsJoined signals

    putStrLn "getting game info..."
    respGameInfo <- Proto.sendRequestSync conn Proto.requestGameInfo
    print respGameInfo
    gameDataResp <- Proto.sendRequestSync conn Proto.requestData
    let gi :: Proto.ResponseGameInfo = respGameInfo ^. #gameInfo
        gd :: Proto.ResponseData = gameDataResp ^. #data'
        -- Just gd = gameDataResp ^. #maybe'data
        _pathingGrid = gridFromImage (gi ^. #startRaw . #pathingGrid)

    --printGrid _pathingGrid
    createDirectoryIfMissing True "grids"
    liftIO $ B.writeFile "grids/gameinfo" (encodeMessage gi)

    obs0 <- Proto.sendRequestSync conn Proto.requestObservation

    agent' <- makeAgent agent localPlayerId gi gd (obs0 ^. #observation)

    gameStepLoop conn agent'

gameStepLoop :: (Agent agent) => Connection -> agent -> IO [PlayerResult]
gameStepLoop conn agent = do
    responseObs <- Proto.sendRequestSync conn Proto.requestObservation
    let gameOver = responseObs ^. #observation . #playerResult

    if not (null gameOver)
        then return gameOver
        else do
            let obs = responseObs ^. #observation . #observation
                _gameLoop = obs ^. #gameLoop

            abilityMap <- unitAbilitiesRaw conn obs <&> unitAbilities
            (agent', StepPlan cmds chats dbgs) <- return $ Agent.agentStep agent (responseObs ^. #observation) abilityMap

            -- (agent', StepPlan cmds chats dbgs, ds') <- return $ runStep si abilities (setObs obs ds) (Agent.agentStep agent)
            -- liftIO $ gridToFile ("grids/grid" ++ show gameLoop ++ ".txt") (getGrid ds')
            -- liftIO $ B.writeFile ("grids/obs" ++ show gameLoop) (encodeMessage obs)
            -- Prelude.putStrLn $ show gameLoop ++ " buildOrder " ++ show bo ++ " queue: " ++ show q
            _ <- liftIO . Proto.sendRequestSync conn $ Proto.requestAction cmds chats
            _ <- liftIO . Proto.sendRequestSync conn . Proto.requestDebug $ dbgs
            _ <- liftIO . Proto.sendRequestSync conn $ Proto.requestStep
            gameStepLoop conn agent'
