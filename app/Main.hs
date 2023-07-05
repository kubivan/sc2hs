{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
-- test

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
import qualified TestProto

testPrint responseMessage = case responseMessage of
  Left errMsg -> print $ "Error decoding message: " ++ errMsg
  Right message -> print $ "Received message: " ++ show message

main :: IO ()
main = startClient

-- main = testEcho

-- main = withSC2 $ \(Client ph conn) -> do
--  putStrLn "Hello, Haskell!"
--  TestProto.test
--  putStrLn "send ping"
--  let ping = TestProto.requestAvailableMap
--  let pingstr = "ping " ++ show ping
--  putStrLn pingstr
--  forever $ putStrLn "blah " >> (WS.sendTextData conn ("blah blah blah blah" :: B.ByteString))
--  WS.sendBinaryData conn $ encodeMessage ping
--  putStrLn "wait response"
--  responseBytes <- WS.receiveData conn
--  let responseMessage = decodeMessage responseBytes :: Either String S.Response
--  testPrint responseMessage
--  -- putStrLn $ "Recieved" ++ show $ testPrint responseMessage
--  return ()

testEcho :: IO ()
testEcho = do
  WS.runClient "127.0.0.1" 9160 "" $ \conn -> forever $ do
    -- WS.sendTextData conn $ ("qqq" :: Text)
    let ping = TestProto.requestAvailableMaps
    WS.sendBinaryData conn $ encodeMessage ping
    msg <- WS.receiveData conn
    T.putStrLn msg

-- responseMaps <- WS.receiveData conn
-- testPrint $ decodeMessage responseMaps

-- WS.sendBinaryData conn $ encodeMessage TestProto.requestPing
-- responsePing <- WS.receiveData conn
-- testPrint $ decodeMessage responsePing

-- return $ newClient ph conn

-- connect = runClient host port "/sc2api" $ \con -> return $ newClient ph con
