{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SC2 (startStarCraft, withSC2, startClient, Client (..)) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import Control.Monad
import Data.ByteString qualified as B
import Data.ProtoLens.Encoding
import Network.WebSockets as WS
import Proto.S2clientprotocol.Sc2api as S
import Proto.S2clientprotocol.Sc2api_Fields as S
import System.Directory
import System.FilePath
import System.IO
import System.Process
import TestProto qualified

host = "127.0.0.1"

port = 8167

testPrint :: Either String S.Response -> IO ()
testPrint responseMessage = case responseMessage of
  Left errMsg -> print $ "Error decoding message: " ++ errMsg
  Right message -> print $ "Received message: " ++ show message

withSC2 :: (Client -> IO a) -> IO a
withSC2 = undefined

-- withSC2 = bracket startClient (\(Client ph con) -> waitForProcess ph)

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

newClient :: ProcessHandle -> WS.Connection -> Client
newClient ph con = Client {processHandle = ph, connection = con}

startClient :: IO ()
startClient = do
  ph <- startStarCraft
  -- tryConnect 60
  threadDelay 30000000
  let opts = WS.defaultConnectionOptions -- {connectionSentClose = False}
  let connectOpts = connect opts
  WS.runClient host port "/sc2api" clientApp
  where
    clientApp conn = forever $ do
      putStrLn "ping"
      WS.sendTextData conn $ B.drop 3 (encodeMessage TestProto.requestPing)
      -- send conn TestProto.requestPing
      -- send conn (ControlMessage (Ping "pinping"))
      responsePing <- WS.receiveData conn
      testPrint $ decodeMessage responsePing

      -- putStrLn "ping2"
      -- WS.sendTextData conn $ encodeMessage TestProto.requestPing
      -- responsePing2 <- WS.receiveData conn
      -- testPrint $ decodeMessage responsePing2

      putStrLn "maps"
      WS.sendBinaryData conn $ encodeMessage TestProto.requestAvailableMaps
      responseMaps <- WS.receiveData conn
      testPrint $ decodeMessage responseMaps

    --
    -- WS.sendBinaryData conn $ encodeMessage TestProto.requestPing
    -- responsePing <- WS.receiveData conn
    -- testPrint $ decodeMessage responsePing

    -- responseMaps <- WS.receiveData conn
    -- testPrint $ decodeMessage responseMaps

    -- WS.sendBinaryData conn $ encodeMessage TestProto.requestPing
    -- responsePing <- WS.receiveData conn
    -- testPrint $ decodeMessage responsePing

    -- return $ newClient ph conn

    -- connect = runClient host port "/sc2api" $ \con -> return $ newClient ph con
    connect opts = WS.runClientWith host port "/sc2api" opts

-- tryConnect retries
--   | retries > 0 = connect (const (pure ())) `catch` (\(x :: SomeException) -> tryAgain (retries - 1))
--   | otherwise = connect (const (pure ()))
-- tryAgain i = putStrLn "retry" >> threadDelay 5000000 >> tryConnect (i - 1)

-- conn <- WS.connect "localhost" 8167 "/sc2api"
