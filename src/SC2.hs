{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module SC2 (runGame, Proto.Participant(..)) where

import Actions
import Agent
import Observation
import Control.Exception
import Control.Monad
import Control.Monad.Writer.Strict
import Control.Monad.Except(ExceptT(..), runExceptT, throwError)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Data.ByteString qualified as B
import Data.ProtoLens.Encoding ( decodeMessage, encodeMessage )
import Network.WebSockets as WS
    ( Connection, runClient, receiveData, sendBinaryData )
import Proto qualified

import Proto.S2clientprotocol.Sc2api as S
    ( PlayerResult, Response, Request, ResponseGameInfo )
import Proto.S2clientprotocol.Data as S
import Proto.S2clientprotocol.Sc2api_Fields as S
import Proto.S2clientprotocol.Common as S
import Proto.S2clientprotocol.Common_Fields as S
import Proto.S2clientprotocol.Query as S
import Proto.S2clientprotocol.Query_Fields as S (abilities, result)
import System.Directory
import System.FilePath
import System.IO
import System.Process
import Conduit ( sinkList, yieldMany, mapC, (.|), runConduitPure )
import Data.Conduit.List (catMaybes)
import Data.Functor((<&>))
import Grid.Grid

import System.IO.Error (tryIOError)

import Data.Bifunctor (first)

import Lens.Micro((&), (.~), (^.))
import Lens.Micro.Extras(view)
import Data.ProtoLens.Labels ()

import Data.Either (either)

import AbilityId
import UnitTypeId
import Proto.S2clientprotocol.Raw_Fields (placementGrid, terrainHeight)

import qualified Data.Vector as V
import Utils (tilePos, distSquared)

import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (isNothing, fromJust)
import Proto (Participant, requestJoinGame1vs1, requestJoinGameVsAi)
import Footprint(getFootprint)
import Data.List (sortOn)
import Units (unitTypeC, runC)
import Debug.Trace (trace)
import Proto.S2clientprotocol.Sc2api_Fields (basePort, playerId)
import GHC.Word
import qualified Proto.S2clientprotocol.Common as A
import qualified Network.WebSockets as A
import qualified GHC.Int
import Data.Int (Int32)
import Grid.Utils (gridMerge, pixelIsRamp)

hostName :: String
hostName = "127.0.0.1"
--port :: Int
--port = 8167
portHost :: GHC.Int.Int32
portHost = 8167
portClient :: GHC.Int.Int32
portClient = portHost + 1 --8168

serverPortSet :: (Int32, Int32)
serverPortSet = (portHost + 2, portHost + 3)
clientPortSet :: (Int32, Int32)
clientPortSet = (portHost + 4, portHost + 5)

startStarCraft :: Int32 -> IO ProcessHandle
startStarCraft port = do
  let sc2 = "\"C:\\Program Files (x86)\\StarCraft II\\Versions\\Base93333\\SC2_x64.exe\""
      cwd = Just "C:\\Program Files (x86)\\StarCraft II\\Support64"
      args = ["-listen", hostName, "-port", show port, "-displayMode", "0", "-windowwidth", "1024", "-windowheight", "768", "-windowx", "100", "-windowy", "200"]
      proc = (shell $ sc2 ++ " " ++ unwords args) {cwd = cwd}

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
        Right r -> trace (show r)return True

      --return $ either (const False) (const True) result

    -- Function to wait until the SC2 WebSocket connection is available
    waitForSC2WebSocket :: String -> Int32 -> IO ()
    waitForSC2WebSocket host port = loop where
      loop = do
        isOpen <- isWebSocketOpen host (fromIntegral port)
        if isOpen
          then putStrLn "SC2 WebSocket connection is open and ready."
          else do
            threadDelay pollInterval
            loop

unitAbilitiesRaw :: WS.Connection -> Observation -> IO [S.ResponseQueryAvailableAbilities]
unitAbilitiesRaw conn obs = do
  resp <- Proto.sendRequestSync conn $ Proto.requestUnitAbilities obs
  return $ resp ^. #query . #abilities

unitAbilities :: [S.ResponseQueryAvailableAbilities] -> UnitAbilities
unitAbilities raw = HashMap.fromList . runConduitPure $ yieldMany raw
    .| mapC (\a ->
            let unit = UnitTypeId.toEnum (fromIntegral (a ^. #unitTypeId)) :: UnitTypeId
                abilityIds = [ AbilityId.toEnum . fromIntegral $ x ^. #abilityId :: AbilityId | x <- a ^. #abilities]
            in
            (unit, abilityIds)
        )
    .| sinkList

unitsData :: [S.UnitTypeData] -> UnitTraits
unitsData raw = HashMap.fromList . runConduitPure $ yieldMany raw
    .| mapC (\a -> (UnitTypeId.toEnum . fromIntegral $ a ^. #unitId, a))
    .| sinkList

gameStepLoop :: Agent agent d => Connection -> Agent.StaticInfo -> d -> agent -> IO [S.PlayerResult]
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
      --liftIO $ gridToFile ("grids/grid" ++ show gameLoop ++ ".txt") (getGrid ds')
      --liftIO $ B.writeFile ("grids/obs" ++ show gameLoop) (encodeMessage obs)
      --Prelude.putStrLn $ show gameLoop ++ " buildOrder " ++ show bo ++ " queue: " ++ show q
      _ <- liftIO . Proto.sendRequestSync conn $ Proto.requestAction cmds chats
      _ <- liftIO . Proto.sendRequestSync conn . Proto.requestDebug $ dbgs
      _ <- liftIO . Proto.sendRequestSync conn $ Proto.requestStep
      gameStepLoop conn si ds' agent'

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
signalClientJoined signals = trace "signalClientJoined" modifyMVar_ (allClientsJoined signals) (return.(+1))

-- Wait for all clients to join
waitAllClientsJoined :: GameSignals -> IO ()
waitAllClientsJoined signals = do
  trace "waitForAllClients: " (return ())
  joinedCountNotMeet
  where
    joinedCountNotMeet :: IO ()
    joinedCountNotMeet = do
      count <- readMVar $ allClientsJoined signals
      let expectedCount = expectedClients signals
      --trace ("waitForAllClients: current count: " ++ show count ++ "expected: " ++ show expectedCount) (return ())
      when (count < expectedCount) (threadDelay 500 >> joinedCountNotMeet)

-- Wait for the host's game creation signal
waitForGameCreation :: GameSignals -> IO ()
waitForGameCreation signals = do_wait
  where
    do_wait = do
      trace "waitForGameCreation" return ()
      created <- readMVar (gameCreated signals)
      unless created (threadDelay 500 >> do_wait)

runHost :: Proto.Participant -> [Proto.Participant] -> GameSignals -> (A.Race -> S.Request) -> IO [PlayerResult]
runHost (Proto.Computer _) _ _ _ = Prelude.error "computer cannot be the host"
runHost (Proto.Player agent) participants signals joinFunc = do
  trace "starting sc2 host" return ()
  _ <- startStarCraft portHost
  -- tryConnect 60
  threadDelay 25000000

  trace "running host" return ()
  WS.runClient hostName (fromIntegral portHost) "/sc2api" clientApp
  where
    clientApp conn = do
      --TODO: move to separate
      --putStrLn "maps"
      --responseMaps <- Proto.sendRequestSync conn Proto.requestAvailableMaps
      --print responseMaps

      putStrLn "creating game..."
      responseCreateGame <- Proto.sendRequestSync conn $ Proto.requestCreateGame (Proto.LocalMap "ai/2000AtmospheresAIE.SC2Map" Nothing) participants
      print responseCreateGame
      signalGameCreated signals

      putStrLn "Host joining game..."
      responseJoinGame <- Proto.sendRequestSync conn $ joinFunc (Agent.agentRace agent)
      print responseJoinGame
      let playerId = responseJoinGame ^. #joinGame . #playerId
      trace ("playerId " ++ show playerId) return ()
      signalClientJoined signals

      runGameLoop conn signals agent playerId

runGameLoop :: Agent a d => Connection -> GameSignals -> a -> Word32 -> IO [PlayerResult]
runGameLoop conn signals agent playerId = do

  putStrLn $ "Player" ++ show playerId ++ "waiting for host to create the game..."
  waitAllClientsJoined signals

  putStrLn "getting game info..."
  resp <- Proto.sendRequestSync conn Proto.requestGameInfo
  let gi :: S.ResponseGameInfo = resp ^. #gameInfo
      playerInfos = gi ^. #playerInfo
      playerGameInfo = head $ filter (\gi -> gi ^. #playerId == playerId ) playerInfos
      pathingGrid = gridFromImage (gi ^. #startRaw . #pathingGrid)


  printGrid pathingGrid
  liftIO $ B.writeFile "grids/gameinfo" (encodeMessage gi)

  gameDataResp <- Proto.sendRequestSync conn Proto.requestData
  obs0 <- Proto.sendRequestSync conn Proto.requestObservation

  let heightMap = gridFromImage $ gi ^. #startRaw . #terrainHeight
      obsRaw = obs0 ^. #observation . #observation
      unitTraits = unitsData $ gameDataResp ^. #data' . #units
      grid = gridMerge pixelIsRamp (gridFromImage $ gi ^. #startRaw . #placementGrid) (gridFromImage $ gi ^. #startRaw . #pathingGrid)
      gridPlacement = gridFromImage $ gi ^. #startRaw . #placementGrid
      nexusPos = view #pos $ head $ runC $ unitsSelf obsRaw .| unitTypeC ProtossNexus
      expands = sortOn (distSquared nexusPos) $ findExpands obsRaw grid heightMap
      enemyStart = tilePos $ enemyBaseLocation gi obsRaw
      si = Agent.StaticInfo gi playerGameInfo unitTraits heightMap expands enemyStart

      nexusCenter = gridPixel gridPlacement (tilePos nexusPos)

  print $ "!!! nexusCenter is: " ++ show nexusCenter ++ " at " ++ show (tilePos nexusPos)
  trace "create ds" return ()
  dynamicState <- makeDynamicState agent obsRaw grid
  trace "created ds" return ()

  gameStepLoop conn si dynamicState agent

clientRunGame :: Proto.Participant -> GameSignals -> (A.Race -> Request) -> IO [PlayerResult]
clientRunGame (Proto.Computer _) signals _ = Prelude.error "!!! this should never happend: computer has not been split out."
clientRunGame (Proto.Player agent) signals joinFunc = do
  trace "starting sc2 for second player" return ()
  threadId <- myThreadId
  putStrLn $ "Current thread ID: " ++ show threadId

  _ <- startStarCraft portClient
  trace "second sc2 started" return ()

  waitForGameCreation signals
  trace "running clientRunGame" return ()
  WS.runClient hostName (fromIntegral portClient) "/sc2api" clientApp
  where
    clientApp conn = do
      trace "running clientRunGame clientApp conn" return ()
      putStrLn "Client joining game..."
      responseJoinGame <- Proto.sendRequestSync conn (joinFunc (Agent.agentRace agent))
      print responseJoinGame
      let playerId = responseJoinGame ^. #joinGame . #playerId
      trace ("playerId " ++ show playerId) return ()
      signalClientJoined signals
      runGameLoop conn signals agent playerId

runGame :: [Proto.Participant] -> IO ()
runGame participants = do
  let (host, players, joinFunc) = splitParticipants participants
  signals <- newGameSignals (1 + length players)
  asyncHostTask <- async $ do
    threadId <- myThreadId
    putStrLn $ "Current thread ID: " ++ show threadId
    runHost host participants signals joinFunc

  asyncClientTasks <- mapM (\p -> async $ clientRunGame p signals joinFunc) players

  gameOver <- wait asyncHostTask
  mapM_ wait asyncClientTasks
  putStrLn $ "Game Ended :" ++ show gameOver

--data AnyAgent = forall a. Agent a => AnyAgent a
--data AnyAgent = forall a dyn. (Agent a dyn) => AnyAgent a dyn
--getAgents :: [Proto.Participant] -> [AnyAgent]
--getAgents participants = [AnyAgent a | Proto.Player a <- participants]
--
-- host/players
splitParticipants :: [Proto.Participant] -> (Proto.Participant, [Proto.Participant], Race -> S.Request)
splitParticipants = doSplit (Nothing, [], Nothing) where
  doSplit :: (Maybe Proto.Participant, [Proto.Participant], Maybe (Race -> S.Request)) -> [Proto.Participant] -> (Proto.Participant, [Proto.Participant], Race -> S.Request)
  doSplit (host, players, joinFunc) (r:rest) = case r of
    Proto.Player {}  -> if isNothing host then doSplit (Just r, players, joinFunc) rest
      else doSplit (host, r:players, Just $ requestJoinGame1vs1 serverPortSet clientPortSet) rest
    Proto.Computer {} -> doSplit (host, players, Just requestJoinGameVsAi) rest
  doSplit (Just h, p, j) [] = (h, p, fromJust j)
  doSplit _ [] = Prelude.error "There should be atleast one Bot player!"
