{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs #-}

module Proto ( requestPing
             , requestAvailableMaps
             , requestCreateGame
             , Map(LocalMap)
             --TODO: remove code duplication
             , requestJoinGame1vs1
             , requestJoinGameVsAi
             , requestObservation
             , requestStep
             , requestAction
             , requestDebug
             , requestGameInfo
             , requestData
             , requestUnitAbilities
             , sendRequestSync
             , Participant(..)) where

import Actions
import Agent

import Network.WebSockets as WS

import Control.Exception

import Data.ByteString qualified as BS
import Data.ProtoLens (defMessage, encodeMessage, decodeMessage)
import Data.ProtoLens.Labels ()
import Data.ProtoLens.Field (field)
--import Data.ProtoLens.Prism qualified as P
import Data.Text
import Lens.Micro((&), (.~), (^.))
import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import Proto.S2clientprotocol.Sc2api as A
import Proto.S2clientprotocol.Sc2api_Fields as A
import Proto.S2clientprotocol.Query_Fields (ignoreResourceRequirements, abilities, unitTag, unitTypeId)

import Conduit
import Proto.S2clientprotocol.Raw_Fields (alliance)
import qualified Proto.S2clientprotocol.Raw as A
import qualified Proto.S2clientprotocol.Query as A

import GHC.Word (Word64)
import Data.ByteString (ByteString)
import GHC.Int (Int32)
import Utils
import Debug.Trace (trace)

newtype ProtoException = ProtoException String deriving Show
instance Exception ProtoException

makeException :: String -> ProtoException
makeException = ProtoException

decodeMessageThrowing :: BS.ByteString -> IO A.Response
decodeMessageThrowing msg = either (throwIO . makeException) return (decodeMessage msg)

sendRequestSync :: WS.Connection -> A.Request -> IO A.Response
sendRequestSync conn request = WS.sendBinaryData conn (encodeMessage request) >> WS.receiveData conn >>= decodeMessageThrowing --`Utils.dbg` show request

requestAvailableMaps :: A.Request
requestAvailableMaps = defMessage & #availableMaps .~ defMessage & #id .~ 123

requestPing :: A.Request
requestPing = defMessage
  & #ping .~ defMessage & #id .~ 123

data Map = LocalMap Text (Maybe BS.ByteString)

data Participant where
    Computer :: C.Race -> Participant
    Player :: Agent a d => a -> Participant

requestCreateGame:: Map -> [Participant] -> A.Request
requestCreateGame lm@(LocalMap m d) participants = defMessage& #createGame .~ mods defMessage
  where
    mods = setMap lm . setFog . setRealTime . setPlayers participants

    setFog :: A.RequestCreateGame -> A.RequestCreateGame
    setFog = #disableFog .~ False

    setRealTime :: A.RequestCreateGame -> A.RequestCreateGame
    setRealTime = #realtime .~ False

    setMap :: Map -> A.RequestCreateGame -> A.RequestCreateGame
    setMap (LocalMap m d) = #localMap .~ (defMessage & #mapPath .~ m & #maybe'mapData .~ d)

    setPlayers :: [Participant] -> A.RequestCreateGame -> A.RequestCreateGame
    setPlayers participants = #playerSetup .~ (toPlayerSetup <$> participants)

    toPlayerSetup :: Participant -> A.PlayerSetup
    toPlayerSetup (Proto.Computer r) = defMessage & #race .~ r & #type' .~ A.Computer & #difficulty .~ A.Hard --TODO: computer
    toPlayerSetup (Player agent) = defMessage & #race .~ Agent.agentRace agent & #type' .~ Participant

requestJoinGame1vs1 :: (Int32, Int32) -> (Int32, Int32) -> C.Race -> A.Request
requestJoinGame1vs1 serverPorts clientPorts race = defMessage & #joinGame .~ mods defMessage
  where
    mods = setParticipation race . setOptions . setPortsServer serverPorts . setPortsClient clientPorts -- . setSharedPort

    setParticipation :: C.Race -> A.RequestJoinGame -> A.RequestJoinGame
    setParticipation r = #race .~ r

    setOptions :: A.RequestJoinGame -> A.RequestJoinGame
    setOptions = #options .~ (defMessage & #raw .~ True & #score .~ True)

    setPortsServer :: (Int32, Int32) -> A.RequestJoinGame -> A.RequestJoinGame
    setPortsServer (gamePort, basePort) = #serverPorts .~ (defMessage & #gamePort .~ gamePort & #basePort .~ basePort)

    setPortsClient :: (Int32, Int32) -> A.RequestJoinGame -> A.RequestJoinGame
    setPortsClient (gamePort, basePort)= #clientPorts .~ [defMessage & #gamePort .~ gamePort & #basePort .~ basePort]

requestJoinGameVsAi :: C.Race -> A.Request
requestJoinGameVsAi race = defMessage & #joinGame .~ mods defMessage
  where
    mods = setParticipation race . setOptions

    setParticipation :: C.Race -> A.RequestJoinGame -> A.RequestJoinGame
    setParticipation r = #race .~ r

    setOptions :: A.RequestJoinGame -> A.RequestJoinGame
    setOptions = #options .~ (defMessage & #raw .~ True & #score .~ True)

requestObservation :: A.Request
requestObservation = defMessage & #observation .~ defMessage

requestStep :: A.Request
requestStep = defMessage & #step .~ (defMessage & #count .~ 1)

requestAction :: [Actions.Action] -> [Actions.ChatMsg] -> A.Request
requestAction acts chats = defMessage & #action .~ ((defMessage & #actions .~ ((toAction <$> acts) ++ (toChatAction <$> chats)))::A.RequestAction)

requestDebug :: [Actions.DebugCommand] -> A.Request
requestDebug acts = defMessage & #debug .~ ((defMessage & #debug .~ (toDebug <$> acts))::A.RequestDebug)

requestGameInfo :: A.Request
requestGameInfo = defMessage & #gameInfo .~ defMessage

requestData :: A.Request
requestData = defMessage & #data' .~ (defMessage & #abilityId .~ True & #unitTypeId .~ True & #upgradeId .~ True & #buffId .~ False & #effectId .~ True)

--TODO: add ignoreResourceRequirements param
requestUnitAbilities :: A.Observation -> A.Request
requestUnitAbilities obs = defMessage & #query .~ requestQueryAbilities obs where
  requestQueryAbilities :: A.Observation -> A.RequestQuery
  requestQueryAbilities obs = defMessage & #abilities .~ unitTags obs
  units :: A.Observation -> [A.Unit]
  units obs = obs ^. (#rawData . #units)
  toRequest:: Word64 -> A.RequestQueryAvailableAbilities
  toRequest ut = defMessage & #unitTag .~ ut
  unitTags :: A.Observation -> [A.RequestQueryAvailableAbilities]
  unitTags obs = runConduitPure $
    yieldMany (units obs)
    .| filterC (\u -> u ^. alliance == A.Self) -- Filter based on some condition (e.g., health > 50)
    .| mapC (\u -> u ^. #tag)

    .| mapC toRequest
    .| sinkList
