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
    unitsData,
) where

import AbilityId (AbilityId, toEnum)
import StepMonad (UnitTraits)
import UnitAbilities
import Observation (Observation)
import Proto qualified
import UnitTypeId (UnitTypeId, toEnum)
import SC2.Config

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
                    let unit = UnitTypeId.toEnum (fromIntegral (a ^. #unitTypeId)) :: UnitTypeId
                        abilityIds = [AbilityId.toEnum . fromIntegral $ x ^. #abilityId :: AbilityId | x <- a ^. #abilities]
                     in (unit, abilityIds)
                )
            .| sinkList

unitsData :: [S.UnitTypeData] -> UnitTraits
unitsData raw =
    HashMap.fromList . runConduitPure $
        yieldMany raw
            .| mapC (\a -> (UnitTypeId.toEnum . fromIntegral $ a ^. #unitId, a))
            .| sinkList

startStarCraft :: Int32 -> IO ProcessHandle
startStarCraft port = do
    let sc2 = "\"C:\\Program Files (x86)\\StarCraft II\\Versions\\Base93333\\SC2_x64.exe\""
        cwd = Just "C:\\Program Files (x86)\\StarCraft II\\Support64"
        args = ["-listen", hostName, "-port", show port, "-displayMode", "0", "-windowwidth", "1024", "-windowheight", "768", "-windowx", "100", "-windowy", "200"]
        proc = (shell $ sc2 ++ " " ++ unwords args){cwd = cwd}

    (_, _, _, sc2Handle) <- createProcess proc
    waitForSC2WebSocket hostName port
    return sc2Handle
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
    waitForSC2WebSocket host port = loop
      where
        loop = do
            isOpen <- isWebSocketOpen host (fromIntegral port)
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
        --traceM ("waitForAllClients: current count: " ++ show count ++ "expected: " ++ show expectedCount)
        when (count < expectedCount) (threadDelay 500 >> joinedCountNotMeet)

-- Wait for the host's game creation signal
waitForGameCreation :: GameSignals -> IO ()
waitForGameCreation signals = do_wait
  where
    do_wait = do
        traceM "waitForGameCreation"
        created <- readMVar (gameCreated signals)
        unless created (threadDelay 500 >> do_wait)
