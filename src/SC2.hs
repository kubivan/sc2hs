{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module SC2 (startClient, Proto.Participant(..)) where

import Actions
import Agent
import Observation

import Control.Concurrent (threadDelay, forkIO)
import Control.Exception
import Control.Monad
import Control.Monad.Writer.Strict
import Control.Monad.Except(ExceptT(..), runExceptT, throwError)
import Data.ByteString qualified as B
import Data.ProtoLens.Encoding ( decodeMessage, encodeMessage )
import Network.WebSockets as WS
    ( Connection, runClient, receiveData, sendBinaryData )
import Proto qualified

import Proto.S2clientprotocol.Sc2api as S
    ( PlayerResult, Response, ResponseGameInfo )
import Proto.S2clientprotocol.Data as S
import Proto.S2clientprotocol.Sc2api_Fields as S
import Proto.S2clientprotocol.Common as S
import Proto.S2clientprotocol.Common_Fields as S
import Proto.S2clientprotocol.Query as S
import Proto.S2clientprotocol.Query_Fields as S (abilities)
import System.Directory
import System.FilePath
import System.IO
import System.Process
import Conduit ( sinkList, yieldMany, mapC, (.|), runConduitPure )
import Data.Conduit.List (catMaybes)
import Data.Functor((<&>))
import Grid

import System.IO.Error (tryIOError)

import Data.Bifunctor (first)

import Lens.Micro((&), (.~), (^.))
import Data.ProtoLens.Labels ()

import Data.Either (either)

import AbilityId
import UnitTypeId
import Proto.S2clientprotocol.Raw_Fields (placementGrid, terrainHeight)

import qualified Data.Vector as V
import Utils (tilePos)

import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (isNothing)
import Proto (Participant)
import Footprint(getFootprint)

hostName :: String
hostName = "127.0.0.1"
port :: Int
port = 8167

testPrint :: Either String S.Response -> IO ()
testPrint responseMessage = case responseMessage of
  Left errMsg -> print ("Error decoding message: " ++ errMsg)
  Right message -> print ("Received message: " ++ show message)

mapIOError :: IO a -> ExceptT String IO a
mapIOError = ExceptT . fmap (first show) . tryIOError

-- TODO: sequence? 
decodeMessage' :: B.ByteString -> ExceptT String IO S.Response
decodeMessage' msg = either throwError return (decodeMessage msg)

receiveData' :: WS.Connection -> ExceptT String IO B.ByteString
receiveData' conn = mapIOError $ WS.receiveData conn

decodeResponseIO :: WS.Connection -> ExceptT String IO S.Response
decodeResponseIO conn = receiveData' conn >>= decodeMessage'

startStarCraft :: Int -> IO ProcessHandle
startStarCraft port = do
  let sc2 = "\"C:\\Program Files (x86)\\StarCraft II\\Versions\\Base92440\\SC2_x64.exe\""
      cwd = Just "C:\\Program Files (x86)\\StarCraft II\\Support64"
      args = ["-listen", hostName, "-port", show port, "-displayMode", "0", "-windowwidth", "1024", "-windowheight", "768", "-windowx", "100", "-windowy", "200"]
      proc = (shell $ sc2 ++ " " ++ unwords args) {cwd = cwd}

  (_, _, _, sc2Handle) <- createProcess proc

  return sc2Handle

sc2Observation :: WS.Connection -> ExceptT String IO (Observation, [S.PlayerResult])
sc2Observation conn = do
  liftIO . WS.sendBinaryData conn . encodeMessage $ Proto.requestObservation
  responseObs <- decodeResponseIO conn
  return (responseObs ^. (#observation . #observation), responseObs ^. (#observation . #playerResult))

unitAbilitiesRaw :: WS.Connection -> Observation -> ExceptT String IO [S.ResponseQueryAvailableAbilities]
unitAbilitiesRaw conn obs = do
  liftIO . WS.sendBinaryData conn . encodeMessage $ Proto.requestUnitAbilities obs
  resp <- decodeResponseIO conn
  return $ resp ^. (#query . #abilities)

unitAbilities :: [S.ResponseQueryAvailableAbilities] -> UnitAbilities
unitAbilities raw = HashMap.fromList . runConduitPure $ yieldMany raw
    .| mapC (\a ->
            let unit = UnitTypeId.toEnum (fromIntegral (a ^. #unitTypeId)) :: UnitTypeId
                abilityIds = [ AbilityId.toEnum . fromIntegral $ (x ^. #abilityId) :: AbilityId | x <- a ^. #abilities]
            in
            (unit, abilityIds)
        )
    .| sinkList

unitsData :: [S.UnitTypeData] -> UnitTraits
unitsData raw = HashMap.fromList . runConduitPure $ yieldMany raw
    .| mapC (\a -> (UnitTypeId.toEnum . fromIntegral $ a ^. #unitId, a))
    .| sinkList

data AnyAgent = forall a. Agent a => AnyAgent a

getAgents :: [Proto.Participant] -> [AnyAgent]
getAgents participants = [AnyAgent a | Proto.Player a <- participants]

-- host/players
splitParticipants :: [Proto.Participant] -> (Proto.Participant, [Proto.Participant])
splitParticipants = doSplit (Nothing, []) where
  doSplit :: (Maybe Proto.Participant, [Proto.Participant]) -> [Proto.Participant] -> (Proto.Participant, [Proto.Participant])
  doSplit (host, players) (r:rest) = case r of
    Proto.Player _  -> if isNothing host then doSplit (Just r, players) rest else doSplit (host, r:players) rest
    Proto.Computer _ -> doSplit (host, players) rest
  doSplit (Just h, p) [] = (h, p)
  doSplit _ [] = Prelude.error "There should be atleast one Bot player!"

mergeGrids:: Grid -> Grid -> Grid
mergeGrids placementGrid pathingGrid =
  V.fromList [ V.fromList [ pixelFunc (gridPixel placementGrid (x,y)) (gridPixel pathingGrid (x,y)) | x <- [0..(gridW placementGrid - 1)]] | y <- [0..(gridH placementGrid - 1)]] where
    pixelFunc placement pathing
      | pathing == ' ' && placement == '#' = '/'
      | otherwise = placement

gameStepLoop :: Agent agent => Connection -> Agent.StaticInfo -> Grid -> agent -> ExceptT String IO [S.PlayerResult]
gameStepLoop conn si grid agent = do
  --liftIO $ putStrLn "step"
  (obs, gameOver) <- sc2Observation conn

  abilities <- unitAbilitiesRaw conn obs <&> unitAbilities

  if not (null gameOver)
    then return gameOver
    else do
      let (agent', StepPlan cmds chats dbgs, grid') = runStep si abilities (obs, grid) (Agent.agentStep agent)
      --liftIO . print $ cmds
      liftIO . agentDebug $ agent'
      let gameLoop = obs ^. #gameLoop
      liftIO $ gridToFile ("grids/grid" ++ show gameLoop ++ ".txt") grid'
      --liftIO $ B.writeFile ("grids/obs" ++ show gameLoop) (encodeMessage obs)
      --Prelude.putStrLn $ show gameLoop ++ " buildOrder " ++ show bo ++ " queue: " ++ show q
      _ <- liftIO . Proto.sendRequestSync conn $ Proto.requestAction cmds chats
      _ <- liftIO . Proto.sendRequestSync conn . Proto.requestDebug $ dbgs
      _ <- liftIO . Proto.sendRequestSync conn $ Proto.requestStep
      gameStepLoop conn si grid' agent'

startClient :: [Proto.Participant] -> IO ()
startClient participants = runHost host where
  (host, players) = splitParticipants participants
  runHost (Proto.Computer _) = Prelude.error "computer cannot be the host"
  runHost (Proto.Player agent) = do
    ph <- startStarCraft port
    -- tryConnect 60
    threadDelay 30000000
    WS.runClient hostName port "/sc2api" clientApp
    where
      clientApp conn = do
        -- putStrLn "ping"
        -- WS.sendTextData conn $ B.drop 3 (encodeMessage Proto.requestPing)
        -- responsePing <- WS.receiveData conn
        -- print responsePing

        putStrLn "maps"
        responseMaps <- Proto.sendRequestSync conn Proto.requestAvailableMaps
        print responseMaps

        putStrLn "creating game..."
        responseCreateGame <- Proto.sendRequestSync conn $ Proto.requestCreateGame  (Proto.LocalMap "ai/2000AtmospheresAIE.SC2Map" Nothing) participants
        print responseCreateGame

        putStrLn "joining game..."
        responseJoinGame <- Proto.sendRequestSync conn . Proto.requestJoinGame $ Agent.agentRace agent
        let playerId = responseJoinGame ^. (#joinGame . #playerId)

        putStrLn "getting game info..."
        resp <- Proto.sendRequestSync conn Proto.requestGameInfo
        let gi :: S.ResponseGameInfo = resp ^. #gameInfo
        let playerInfos = gi ^. #playerInfo
        let playerGameInfo = head $ filter (\gi -> gi ^. #playerId == playerId ) playerInfos
        let pathingGrid = gridFromImage (gi ^. (#startRaw . #pathingGrid))
        printGrid pathingGrid
        liftIO $ B.writeFile "grids/gameinfo" (encodeMessage gi)

        gameDataResp <- Proto.sendRequestSync conn Proto.requestData
        obs0 <- Proto.sendRequestSync conn Proto.requestObservation

        let heightMap = gridFromImage $ gi ^. (#startRaw . #terrainHeight)
        let unitTraits = unitsData $ gameDataResp ^. (#data' . #units)
        let grid = mergeGrids (gridFromImage $ gi ^. (#startRaw . #placementGrid)) (gridFromImage $ gi ^. (#startRaw . #pathingGrid))
        let expands = findExpands (obs0 ^. (#observation . #observation)) grid heightMap
        let si = Agent.StaticInfo gi playerGameInfo unitTraits heightMap expands

        gameOver <- runExceptT $ gameStepLoop conn si grid agent
        case gameOver of
          Left e -> putStrLn $ "game failed: " ++ e
          Right gameResults -> putStrLn $ "Game Ended :" ++ show gameResults

-- tryConnect retries
--   | retries > 0 = connect (const (pure ())) `catch` (\(x :: SomeException) -> tryAgain (retries - 1))
--   | otherwise = connect (const (pure ()))
-- tryAgain i = putStrLn "retry" >> threadDelay 5000000 >> tryConnect (i - 1)
