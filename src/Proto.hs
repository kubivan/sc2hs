{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Proto (test, requestPing, requestAvailableMaps, requestCreateGame, Map(LocalMap), requestJoinGame) where

import Data.ByteString qualified as BS
import Data.ProtoLens (defMessage, showMessage)
import Data.ProtoLens.Labels ()
--import Data.ProtoLens.Prism qualified as P
import Data.Text
import Lens.Micro
import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import Proto.S2clientprotocol.Sc2api as A
import Proto.S2clientprotocol.Sc2api_Fields as A

-- import Data.ProtoLens (defMessage, showMessage)

testPoint :: C.Point2D
testPoint = defMessage & C.x .~ 10 & C.y .~ 5

requestAvailableMaps :: A.Request
requestAvailableMaps = defMessage & #availableMaps .~ defMessage & #id .~ 123

requestPing :: A.Request
requestPing = defMessage 
  & #ping .~ defMessage & #id .~ 123

data Map = LocalMap Text (Maybe BS.ByteString)

test :: IO ()
test = putStrLn . showMessage $ testPoint

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
bot =  defMessage & #race .~ C.Protoss & #type' .~ Participant

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
    setPlayers = #playerSetup .~ [bot]

requestJoinGame :: A.Request
requestJoinGame = defMessage & #joinGame .~ mods defMessage 
  where
    mods = setParticipation . setOptions

    setParticipation :: A.RequestJoinGame -> A.RequestJoinGame
    setParticipation = #race .~ Protoss

    setOptions :: A.RequestJoinGame -> A.RequestJoinGame
    setOptions =  #options .~ (defMessage & #raw .~ True & #score .~ True)
