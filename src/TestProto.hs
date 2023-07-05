{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TestProto (test, requestPing, requestAvailableMaps) where

import Data.ProtoLens (defMessage, showMessage)
import Data.ProtoLens.Labels ()
import qualified Data.ProtoLens.Prism as P
import Lens.Micro
import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import Proto.S2clientprotocol.Sc2api
import Proto.S2clientprotocol.Sc2api_Fields

-- import Data.ProtoLens (defMessage, showMessage)

testPoint :: C.Point2D
testPoint = defMessage & C.x .~ 10 & C.y .~ 5

requestAvailableMaps :: Request
requestAvailableMaps = defMessage & #availableMaps .~ defMessage & #id .~ 123

requestPing :: Request
requestPing = defMessage & #ping .~ defMessage & #id .~ 123

test :: IO ()
test = putStrLn . showMessage $ testPoint
