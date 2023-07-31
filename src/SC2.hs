{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}

module SC2 (startStarCraft, startClient) where

import Actions
import Bot

import Control.Concurrent (forkIO, threadDelay)
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
import System.Directory
import System.FilePath
import System.IO
import System.Process

import System.IO.Error (tryIOError)

import Data.Bifunctor (first)

import Lens.Micro((&), (.~), (^.))
import Data.ProtoLens.Labels ()

import Data.Either (either)

host = "127.0.0.1"
port = 8167

testPrint :: Either String S.Response -> IO ()
testPrint responseMessage = case responseMessage of
  Left errMsg -> print $ ("Error decoding message: " ++ errMsg)
  Right message -> print $ ("Received message: " ++ show message)

mapIOError :: IO a -> ExceptT String IO a
mapIOError = ExceptT . fmap (first (\_ -> "TODO: map errors")) . tryIOError

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
      args = ["-listen", host, "-port", show port, "-displayMode", "0", "-windowwidth", "1024", "-windowheight", "768", "-windowx", "100", "-windowy", "200"]
      proc = (shell $ sc2 ++ " " ++ unwords args) {cwd = cwd}

  (_, _, _, sc2Handle) <- createProcess proc

  return sc2Handle

data Client = Client
  { processHandle :: ProcessHandle,
    connection :: WS.Connection
  }

startClient :: Bot bot => bot -> IO ()
startClient initialBot = do
  ph <- startStarCraft
  -- tryConnect 60
  threadDelay 30000000
  let opts = WS.defaultConnectionOptions -- {connectionSentClose = False}
  let connectOpts = connect opts
  WS.runClient host port "/sc2api" clientApp
  where
    clientApp conn = do
      putStrLn "ping"
      WS.sendTextData conn $ B.drop 3 (encodeMessage Proto.requestPing)
      responsePing <- WS.receiveData conn
      testPrint $ decodeMessage responsePing

      putStrLn "maps"
      WS.sendBinaryData conn $ encodeMessage Proto.requestAvailableMaps
      responseMaps <- WS.receiveData conn
      testPrint $ decodeMessage responseMaps

      putStrLn "creating game..."
      WS.sendBinaryData conn $ encodeMessage (Proto.requestCreateGame (Proto.LocalMap "ai/2000AtmospheresAIE.SC2Map" Nothing))
      responseCreateGame <- WS.receiveData conn
      testPrint $ decodeMessage responseCreateGame

      putStrLn "joining game..."
      let requestJoin = encodeMessage Proto.requestJoinGame
      print requestJoin
      WS.sendBinaryData conn $ requestJoin
      responseJoinGame <- WS.receiveData conn
      testPrint $ decodeMessage responseJoinGame

      gameRes <- runExceptT $ gameLoop conn initialBot
      case gameRes of
        Left e -> putStrLn $ "game failed: " ++ e
        Right _ -> return ()

    connect opts = WS.runClientWith host port "/sc2api" opts
    gameLoop :: Bot bot => Connection -> bot -> ExceptT String IO ()
    gameLoop conn bot = do
      liftIO . putStrLn $ "observation game..."
      liftIO . (WS.sendBinaryData conn) . encodeMessage $ Proto.requestObservation

      responseObs <- decodeResponseIO conn
      let obs = responseObs ^. #observation ^. observation
      let (newBot, actions) = runWriter (Bot.step bot obs)
          
      liftIO . (WS.sendBinaryData conn) . encodeMessage $ Proto.requestAction actions
      liftIO . (WS.sendBinaryData conn) . encodeMessage $ Proto.requestStep
      gameLoop conn newBot

-- tryConnect retries
--   | retries > 0 = connect (const (pure ())) `catch` (\(x :: SomeException) -> tryAgain (retries - 1))
--   | otherwise = connect (const (pure ()))
-- tryAgain i = putStrLn "retry" >> threadDelay 5000000 >> tryConnect (i - 1)
