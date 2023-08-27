{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds         #-}

module Proto ( requestPing
             , requestAvailableMaps
             , requestCreateGame
             , Map(LocalMap)
             , requestJoinGame
             , requestObservation
             , requestStep
             , requestAction
             , requestGameInfo
             , requestUnitAbilities) where

import Actions

import Data.ByteString qualified as BS
import Data.ProtoLens (defMessage, showMessage)
import Data.ProtoLens.Labels ()
import Data.ProtoLens.Field (field)
--import Data.ProtoLens.Prism qualified as P
import Data.Text
import Lens.Micro((&), (.~), (^.))
import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import Proto.S2clientprotocol.Sc2api as A
import Proto.S2clientprotocol.Sc2api_Fields as A
import Proto.S2clientprotocol.Query_Fields (ignoreResourceRequirements, abilities, unitTag)

import Conduit
import Proto.S2clientprotocol.Raw_Fields (alliance)
import qualified Proto.S2clientprotocol.Raw as A
import qualified Proto.S2clientprotocol.Query as A

import GHC.Word (Word64)

requestAvailableMaps :: A.Request
requestAvailableMaps = defMessage & #availableMaps .~ defMessage & #id .~ 123

requestPing :: A.Request
requestPing = defMessage
  & #ping .~ defMessage & #id .~ 123

data Map = LocalMap Text (Maybe BS.ByteString)

--data Race = Terran
--          | Zerg
--          | Protoss
--          deriving (Show, Eq)
--
--data PlayerType r = Observer
--              | Participant Proto.Race
--              | Computer Proto.Race A.Difficulty
--              deriving (Show, Eq)

bot :: A.PlayerSetup
bot = defMessage & #race .~ C.Protoss & #type' .~ Participant

opponent :: A.PlayerSetup
opponent = defMessage & #race .~ C.Protoss & #type' .~ A.Computer & #difficulty .~ A.Hard

requestCreateGame:: Map -> A.Request
requestCreateGame lm@(LocalMap m d) = defMessage& #createGame .~ mods defMessage
  where
    mods = setMap lm . setFog . setRealTime . setPlayers

    setFog :: A.RequestCreateGame -> A.RequestCreateGame
    setFog = #disableFog .~ False

    setRealTime :: A.RequestCreateGame -> A.RequestCreateGame
    setRealTime = #realtime .~ False

    setMap :: Map -> A.RequestCreateGame -> A.RequestCreateGame
    setMap (LocalMap m d) = #localMap .~ (defMessage & #mapPath .~ m & #maybe'mapData .~ d)

    setPlayers :: A.RequestCreateGame -> A.RequestCreateGame
    setPlayers = #playerSetup .~ [bot, opponent]

requestJoinGame :: A.Request
requestJoinGame = defMessage & #joinGame .~ mods defMessage
  where
    mods = setParticipation . setOptions

    setParticipation :: A.RequestJoinGame -> A.RequestJoinGame
    setParticipation = #race .~ Protoss

    setOptions :: A.RequestJoinGame -> A.RequestJoinGame
    setOptions =  #options .~ (defMessage & #raw .~ True & #score .~ True)

requestObservation :: A.Request
requestObservation = defMessage & #observation .~ defMessage

requestStep :: A.Request
requestStep = defMessage & #step .~ (defMessage & #count .~ 1)

requestAction :: [Actions.Action] -> A.Request
requestAction acts = defMessage & #action .~ ((defMessage & #actions .~ (toAction <$> acts))::A.RequestAction)

requestGameInfo :: A.Request
requestGameInfo = defMessage & #gameInfo .~ defMessage

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
    -- .| concatMapC (\u -> [u ^. #unitTag]) -- Use concatMapC to flatten the lists
    .| mapC toRequest
    .| sinkList