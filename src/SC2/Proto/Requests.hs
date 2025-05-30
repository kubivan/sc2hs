{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module SC2.Proto.Requests (
    requestPing,
    requestAvailableMaps,
    requestCreateGame,
    -- TODO: remove code duplication
    requestJoinGame1vs1,
    requestJoinGameVsAi,
    requestObservation,
    requestStep,
    requestAction,
    requestDebug,
    requestGameInfo,
    requestData,
    requestUnitAbilities,
    sendRequestSync,
) where

import SC2.Proto.Data
import SC2.Participant
import Actions
    ( DebugCommand, Action, ChatMsg, toDebug, toChatAction, toAction )
import Agent ( Agent(agentRace) )

import Conduit
import Control.Exception
import Data.ByteString qualified as BS
import Data.ProtoLens (decodeMessage, defMessage, encodeMessage)
import Data.ProtoLens.Labels ()
import Data.Text
import GHC.Int (Int32)
import GHC.Word (Word64)
import Lens.Micro ((&), (.~), (^.))
import Network.WebSockets as WS
import Proto.S2clientprotocol.Raw_Fields (alliance)

import Proto.S2clientprotocol.Common as A
import Proto.S2clientprotocol.Query as A
import Proto.S2clientprotocol.Raw as A
import Proto.S2clientprotocol.Sc2api as A
import Proto.S2clientprotocol.Sc2api_Fields as A
import Proto.S2clientprotocol.Data as A
import Proto.S2clientprotocol.Sc2api qualified as A
import Proto.S2clientprotocol.Sc2api_Fields qualified as A

import Proto.S2clientprotocol.Sc2api qualified as A
import Proto.S2clientprotocol.Sc2api_Fields qualified as A

newtype ProtoException = ProtoException String deriving (Show)
instance Exception ProtoException

makeException :: String -> ProtoException
makeException = ProtoException

decodeMessageThrowing :: BS.ByteString -> IO A.Response
decodeMessageThrowing msg = either (throwIO . makeException) return (decodeMessage msg)

sendRequestSync :: WS.Connection -> A.Request -> IO A.Response
sendRequestSync conn request = WS.sendBinaryData conn (encodeMessage request) >> WS.receiveData conn >>= decodeMessageThrowing -- `Utils.dbg` show request

requestAvailableMaps :: A.Request
requestAvailableMaps = defMessage & #availableMaps .~ defMessage & #id .~ 123

requestPing :: A.Request
requestPing =
    defMessage
        & #ping
        .~ defMessage
        & #id
        .~ 123

requestCreateGame :: Map -> [Participant] -> A.Request
requestCreateGame lm@(LocalMap m d) participants = defMessage & #createGame .~ mods defMessage
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
    toPlayerSetup (SC2.Participant.Computer r d) = defMessage & #race .~ r & #type' .~ A.Computer & #difficulty .~ d
    toPlayerSetup (Player agent) = defMessage & #race .~ Agent.agentRace agent & #type' .~ Participant

requestJoinGame1vs1 :: (Int32, Int32) -> (Int32, Int32) -> Race -> A.Request
requestJoinGame1vs1 serverPorts clientPorts race = defMessage & #joinGame .~ mods defMessage
  where
    mods = setParticipation race . setOptions . setPortsServer serverPorts . setPortsClient clientPorts -- . setSharedPort
    setParticipation :: Race -> A.RequestJoinGame -> A.RequestJoinGame
    setParticipation r = #race .~ r

    setOptions :: A.RequestJoinGame -> A.RequestJoinGame
    setOptions = #options .~ (defMessage & #raw .~ True & #score .~ True)

    setPortsServer :: (Int32, Int32) -> A.RequestJoinGame -> A.RequestJoinGame
    setPortsServer (gamePort, basePort) = #serverPorts .~ (defMessage & #gamePort .~ gamePort & #basePort .~ basePort)

    setPortsClient :: (Int32, Int32) -> A.RequestJoinGame -> A.RequestJoinGame
    setPortsClient (gamePort, basePort) = #clientPorts .~ [defMessage & #gamePort .~ gamePort & #basePort .~ basePort]

requestJoinGameVsAi :: Race -> A.Request
requestJoinGameVsAi race = defMessage & #joinGame .~ mods defMessage
  where
    mods = setParticipation race . setOptions

    setParticipation :: Race -> A.RequestJoinGame -> A.RequestJoinGame
    setParticipation r = #race .~ r

    setOptions :: A.RequestJoinGame -> A.RequestJoinGame
    setOptions = #options .~ (defMessage & #raw .~ True & #score .~ True)

requestObservation :: A.Request
requestObservation = defMessage & #observation .~ defMessage

requestStep :: A.Request
requestStep = defMessage & #step .~ (defMessage & #count .~ 1)

requestAction :: [Actions.Action] -> [Actions.ChatMsg] -> A.Request
requestAction acts chats = defMessage & #action .~ ((defMessage & #actions .~ ((toAction <$> acts) ++ (toChatAction <$> chats))) :: A.RequestAction)

requestDebug :: [Actions.DebugCommand] -> A.Request
requestDebug acts = defMessage & #debug .~ ((defMessage & #debug .~ (toDebug <$> acts)) :: A.RequestDebug)

requestGameInfo :: A.Request
requestGameInfo = defMessage & #gameInfo .~ defMessage

requestData :: A.Request
requestData = defMessage & #data' .~ (defMessage & #abilityId .~ True & #unitTypeId .~ True & #upgradeId .~ True & #buffId .~ False & #effectId .~ True)

-- TODO: add ignoreResourceRequirements param
requestUnitAbilities :: A.Observation -> A.Request
requestUnitAbilities obs = defMessage & #query .~ requestQueryAbilities obs
  where
    requestQueryAbilities :: A.Observation -> A.RequestQuery
    requestQueryAbilities obs = defMessage & #abilities .~ unitTags obs & #ignoreResourceRequirements .~ True
    units :: A.Observation -> [A.Unit]
    units obs = obs ^. (#rawData . #units)
    toRequest :: Word64 -> A.RequestQueryAvailableAbilities
    toRequest ut = defMessage & #unitTag .~ ut
    unitTags :: A.Observation -> [A.RequestQueryAvailableAbilities]
    unitTags obs =
        runConduitPure $
            yieldMany (units obs)
                .| filterC (\u -> u ^. alliance == A.Self) -- Filter based on some condition (e.g., health > 50)
                .| mapC (\u -> u ^. #tag)
                .| mapC toRequest
                .| sinkList
