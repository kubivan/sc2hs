{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SC2.Client (
    GameSignals (..),
    newGameSignals,
    signalGameCreated,
    waitForGameCreation,
    signalClientJoined,
    waitAllClientsJoined,
    startStarCraft,
    unitAbilitiesRaw,
    unitAbilities,
) where

import Observation (Observation)
import SC2.BotConfig (StarCraft2Config (..))
import SC2.Ids.AbilityId (AbilityId, toEnum)
import SC2.Ids.UnitTypeId (UnitTypeId, toEnum)
import SC2.Proto.Data qualified as Proto
import SC2.Proto.Requests qualified as Proto
import StepMonad (UnitTraits)
import UnitAbilities

import Conduit (mapC, runConduitPure, sinkList, yieldMany, (.|))
import Control.Concurrent (
    MVar,
    modifyMVar_,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    threadDelay,
 )
import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int32)
import Data.ProtoLens.Labels ()
import Debug.Trace (trace, traceM)
import GHC.Int qualified
import Lens.Micro ((^.))
import Network.WebSockets as WS (
    Connection,
    runClient,
 )
import Proto.S2clientprotocol.Data as S (UnitTypeData)
import Proto.S2clientprotocol.Query as S (
    ResponseQueryAvailableAbilities,
 )
import Proto.S2clientprotocol.Sc2api as S (
    Response,
 )
import System.Process (
    CreateProcess (cwd),
    ProcessHandle,
    createProcess,
    shell,
 )

unitAbilitiesRaw :: WS.Connection -> Observation -> IO [S.ResponseQueryAvailableAbilities]
unitAbilitiesRaw conn obs = do
    resp <- Proto.sendRequestSync conn $ Proto.requestUnitAbilities obs
    return $ resp ^. #query . #abilities

unitAbilities :: [S.ResponseQueryAvailableAbilities] -> UnitAbilities
unitAbilities raw =
    HashMap.fromList . runConduitPure $
        yieldMany raw
            .| mapC
                ( \a ->
                    let unit = toEnum (fromIntegral (a ^. #unitTypeId)) :: UnitTypeId
                        abilityIds = [toEnum . fromIntegral $ x ^. #abilityId :: AbilityId | x <- a ^. #abilities]
                     in (unit, abilityIds)
                )
            .| sinkList

startStarCraft :: StarCraft2Config -> String -> Int32 -> IO ()
startStarCraft cfg host port = do
    let sc2Binary = "\"" ++ exePath cfg ++ "\""
        workingDir = Just (SC2.BotConfig.cwd cfg)
        args =
            [ "-listen"
            , host
            , "-port"
            , show port
            , "-displayMode"
            , "0"
            , "-windowwidth"
            , show (windowWidth cfg)
            , "-windowheight"
            , show (windowHeight cfg)
            , "-windowx"
            , show (windowX cfg)
            , "-windowy"
            , show (windowY cfg)
            ]
        proc = (shell $ sc2Binary ++ " " ++ unwords args){System.Process.cwd = workingDir}

    (_, _, _, _sc2Handle) <- createProcess proc
    waitForSC2WebSocket host port
    --return sc2Handle
  where
    pollInterval = 1000000 -- Poll every second (1,000,000 microseconds)

    -- Function to check if the WebSocket connection is open
    isWebSocketOpen :: String -> Int -> IO Bool
    isWebSocketOpen host port = do
        result <- try (WS.runClient host port "/sc2api" (`Proto.sendRequestSync` Proto.requestPing)) :: IO (Either SomeException Response)
        case result of
            Left err -> do
                putStrLn $ "Connection failed: " ++ show err
                return False
            Right r -> trace (show r) return True

    -- return $ either (const False) (const True) result

    -- Function to wait until the SC2 WebSocket connection is available
    waitForSC2WebSocket :: String -> Int32 -> IO ()
    waitForSC2WebSocket targetHost targetPort = loop
      where
        loop = do
            isOpen <- isWebSocketOpen targetHost (fromIntegral targetPort)
            if isOpen
                then putStrLn "SC2 WebSocket connection is open and ready."
            else do
                threadDelay pollInterval
                loop

data GameSignals = GameSignals
    { gameCreated :: MVar Bool
    , allClientsJoined :: MVar Int
    , expectedClients :: Int
    }

-- Initialize the signals (empty `MVar` for blocking behavior)
newGameSignals :: Int -> IO GameSignals
newGameSignals numClients = GameSignals <$> newEmptyMVar <*> newMVar 0 <*> return numClients

-- Signal to clients that the game is created
signalGameCreated :: GameSignals -> IO ()
signalGameCreated signals = do
    putMVar (gameCreated signals) True
    putStrLn "Host: Game creation signaled."

-- Signal to the host that the client has joined
signalClientJoined :: GameSignals -> IO ()
signalClientJoined signals = trace "signalClientJoined" modifyMVar_ (allClientsJoined signals) (return . (+ 1))

-- Wait for all clients to join
waitAllClientsJoined :: GameSignals -> IO ()
waitAllClientsJoined signals = do
    traceM "waitForAllClients: "
    joinedCountNotMeet
  where
    joinedCountNotMeet :: IO ()
    joinedCountNotMeet = do
        count <- readMVar $ allClientsJoined signals
        let expectedCount = expectedClients signals
        -- traceM ("waitForAllClients: current count: " ++ show count ++ "expected: " ++ show expectedCount)
        when (count < expectedCount) (threadDelay 500 >> joinedCountNotMeet)

-- Wait for the host's game creation signal
waitForGameCreation :: GameSignals -> IO ()
waitForGameCreation signals = do_wait
  where
    do_wait = do
        traceM "waitForGameCreation"
        created <- readMVar (gameCreated signals)
        unless created (threadDelay 500 >> do_wait)
