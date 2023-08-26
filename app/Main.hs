{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception

import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.ProtoLens.Encoding
import Data.Text as T
import Data.Text.IO as T
import Network.WebSockets (ResponseHead (responseMessage), WebSocketsData (fromLazyByteString))
import qualified Network.WebSockets as WS
import Proto.S2clientprotocol.Sc2api as S
import Proto.S2clientprotocol.Sc2api_Fields as S
import SC2
import System.Process
import qualified Proto
import Bot
import Data.ProtoLens (Message(defMessage))

testPrint responseMessage = case responseMessage of
  Left errMsg -> print $ "Error decoding message: " ++ errMsg
  Right message -> print $ "Received message: " ++ show message

main :: IO ()
main = startClient $ TestBot {aGameInfo = defMessage, aBotInfo = defMessage}

testEcho :: IO ()
testEcho = do
  WS.runClient "127.0.0.1" 9160 "" $ \conn -> forever $ do
    let ping = Proto.requestAvailableMaps
    WS.sendBinaryData conn $ encodeMessage ping
    msg <- WS.receiveData conn
    T.putStrLn msg