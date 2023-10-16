{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module SC2 (startClient, Proto.Participant(..)) where

import Actions
import Agent

import Control.Concurrent (threadDelay, forkIO)
import Control.Exception
import Control.Monad
import Control.Monad.Writer.Strict
import Control.Monad.Except(ExceptT(..), runExceptT, throwError)
import Data.ByteString qualified as B
import Data.ProtoLens.Encoding
import Network.WebSockets as WS
import Proto qualified
import Proto.S2clientprotocol.Sc2api as S
import Proto.S2clientprotocol.Sc2api_Fields as S
import Proto.S2clientprotocol.Common as S
import Proto.S2clientprotocol.Common_Fields as S
import Proto.S2clientprotocol.Query as S
import Proto.S2clientprotocol.Query_Fields as S ()
import System.Directory
import System.FilePath
import System.IO
import System.Process
import Conduit ( sinkList, yieldMany, mapC, (.|), runConduitPure )
import Data.Conduit.List (catMaybes)
import Grid

import Utils qualified

import System.IO.Error (tryIOError)

import Data.Bifunctor (first)

import Lens.Micro((&), (.~), (^.))
import Data.ProtoLens.Labels ()

import Data.Either (either)

import AbilityId
import UnitTypeId
import Proto.S2clientprotocol.Raw_Fields (placementGrid)

import qualified Data.Vector as V
import Utils (enemyBaseLocation)

import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (isNothing)

hostName :: String
hostName = "127.0.0.1"
port :: Int
port = 8167

testPrint :: Either String S.Response -> IO ()
testPrint responseMessage = case responseMessage of
  Left errMsg -> print ("Error decoding message: " ++ errMsg)
  Right message -> print ("Received message: " ++ show message)

mapIOError :: IO a -> ExceptT String IO a
mapIOError = ExceptT . fmap (first (const "TODO: map errors")) . tryIOError

-- TODO: sequence? 
decodeMessage' :: B.ByteString -> ExceptT String IO S.Response
decodeMessage' msg = either throwError return (decodeMessage msg)

receiveData' :: WS.Connection -> ExceptT String IO B.ByteString
receiveData' conn = mapIOError $ WS.receiveData conn

decodeResponseIO :: WS.Connection -> ExceptT String IO S.Response
decodeResponseIO conn = receiveData' conn >>= decodeMessage'

startStarCraft :: IO ProcessHandle
startStarCraft = do
  let sc2 = "\"C:\\Program Files (x86)\\StarCraft II\\Versions\\Base87702\\SC2_x64.exe\""
      cwd = Just "C:\\Program Files (x86)\\StarCraft II\\Support64"
      args = ["-listen", hostName, "-port", show port, "-displayMode", "0", "-windowwidth", "1024", "-windowheight", "768", "-windowx", "100", "-windowy", "200"]
      proc = (shell $ sc2 ++ " " ++ unwords args) {cwd = cwd}

  (_, _, _, sc2Handle) <- createProcess proc

  return sc2Handle

sc2Observation :: WS.Connection -> ExceptT String IO (S.Observation, [S.PlayerResult])
sc2Observation conn = do
  liftIO . WS.sendBinaryData conn . encodeMessage $ Proto.requestObservation
  responseObs <- decodeResponseIO conn
  return (responseObs ^. (#observation . #observation), responseObs ^. (#observation . #playerResult))

unitAbilitiesRaw :: WS.Connection -> S.Observation -> ExceptT String IO [S.ResponseQueryAvailableAbilities]
unitAbilitiesRaw conn obs = do
  liftIO . WS.sendBinaryData conn . encodeMessage $ Proto.requestUnitAbilities obs
  resp <- decodeResponseIO conn
  return $ resp ^. (#query . #abilities)

unitAbilities :: [S.ResponseQueryAvailableAbilities] -> UnitAbilities
unitAbilities raw = HashMap.fromList $ runConduitPure $ yieldMany raw
    .| mapC (\a ->
            let unit = UnitTypeId.toEnum (fromIntegral (a ^. #unitTypeId)) :: UnitTypeId
                abilityIds = [ AbilityId.toEnum . fromIntegral $ (x ^. #abilityId) :: AbilityId | x <- a ^. #abilities]
            in
            (unit, abilityIds)
        )
    .| sinkList

mirrorGrid :: Grid -> Grid
mirrorGrid grid =
    V.fromList [V.fromList [grid V.! j V.! i | j <- [0..V.length grid - 1]] | i <- [0..V.length (V.head grid) - 1]]

printGrid grid = V.sequence_ $ putStrLn . V.toList <$> (V.reverse . mirrorGrid $ grid)

-- extractAgents :: Agent a => [Proto.Participant] -> [a]
-- extractAgents ps = runConduitPure $ yieldMany ps
--           .| mapC (\x -> case x of Proto.Player a -> Just a; _ -> Nothing)
--           .| catMaybes
--           .| sinkList

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

startClient :: [Proto.Participant] -> IO ()
startClient participants = runHost host >> mapM_ (forkIO . runPlayer) players where
  (host, players) = splitParticipants participants
  runPlayer _ = return ()
  runHost (Proto.Computer _) = Prelude.error "computer cannot be the host"
  runHost (Proto.Player agent) = do
    ph <- startStarCraft
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

        gameOver <- runExceptT $ gameLoop conn gi playerGameInfo agent 0
        case gameOver of
          Left e -> putStrLn $ "game failed: " ++ e
          Right gameResults -> putStrLn $ "Game Ended :" ++ show gameResults

      gameLoop :: Agent agent => Connection -> S.ResponseGameInfo -> PlayerInfo -> agent -> Int -> ExceptT String IO [S.PlayerResult]
      gameLoop conn gameInfo playerInfo bot stepCount = do
          liftIO $ putStrLn "step"
          (obs, gameOver) <- sc2Observation conn

          abilitiesRaw <- unitAbilitiesRaw conn obs

          if not (null gameOver)
              then return gameOver
              else do
                  let (nextAgent, acts) = runWriter (Agent.agentStep agent gameInfo playerInfo obs (unitAbilities abilitiesRaw) stepCount)

                  _ <- liftIO . Proto.sendRequestSync conn . Proto.requestAction . botCommands $ acts
                  _ <- liftIO . Proto.sendRequestSync conn . Proto.requestDebug . botDebug $ acts
                  _ <- liftIO . Proto.sendRequestSync conn $ Proto.requestStep
                  gameLoop conn gameInfo playerInfo nextAgent (stepCount + 1)

-- tryConnect retries
--   | retries > 0 = connect (const (pure ())) `catch` (\(x :: SomeException) -> tryAgain (retries - 1))
--   | otherwise = connect (const (pure ()))
-- tryAgain i = putStrLn "retry" >> threadDelay 5000000 >> tryConnect (i - 1)
