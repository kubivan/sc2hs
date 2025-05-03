{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Actions (Action (..), toAction, toChatAction, UnitTag, ChatMsg, toDebug, DebugCommand (..), getCmd, getTarget, getExecutors) where

import Data.Text
import Lens.Micro ((&), (.~), (^.))

import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Debug qualified as D
import Proto.S2clientprotocol.Raw as R
import Proto.S2clientprotocol.Sc2api qualified as A
import Proto.S2clientprotocol.Sc2api_Fields as A

import AbilityId

import Utils

-- import Proto.S2clientprotocol.Raw_Fields (unitCommand, abilityId)
-- import qualified GHC.Word
import GHC.Word (Word64)
import Proto.S2clientprotocol.Debug qualified as D
import Proto.S2clientprotocol.Raw_Fields (targetUnitTag)

type UnitTag = Word64
type ChatMsg = Text

data Action
    = -- | forall a. Pointable a => PointCommand AbilityId UnitTag a
      PointCommand AbilityId [Unit] Point2D
    | UnitCommand AbilityId [Unit] Unit
    | SelfCommand AbilityId [Unit]
    deriving (Show)

getExecutors :: Action -> [Unit]
getExecutors (UnitCommand _ us _) = us
getExecutors (SelfCommand _ us) = us
getExecutors (PointCommand _ us _) = us

getCmd :: Action -> AbilityId
getCmd (UnitCommand a _ _) = a
getCmd (SelfCommand a _) = a
getCmd (PointCommand a _ _) = a

getTarget :: Action -> Point2D
getTarget (PointCommand _ _ t) = t
getTarget (UnitCommand _ _ t) = toPoint2D (tPos)
  where
    tPos :: Point
    tPos = t ^. #pos

-- // Display debug text on screen.
-- message DebugText {
--   optional Color color = 1;
--   optional string text = 2;         // Text to display.
--   optional Point virtual_pos = 3;   // Virtualized position in 2D (the screen is 0..1, 0..1 for any resolution).
--   optional Point world_pos = 4;     // Position in the world.
--   optional uint32 size = 5;         // Pixel height of the text. Defaults to 8px.
-- }
data DebugCommand = DebugText Text C.Point

toDebug :: DebugCommand -> D.DebugCommand
toDebug (DebugText t p) = defMessage & #draw .~ drawMsg
  where
    drawMsg :: D.DebugDraw
    drawMsg = defMessage & #text .~ [textMsg]
    textMsg :: D.DebugText
    textMsg = defMessage & #text .~ t & #worldPos .~ p

toChatAction :: ChatMsg -> A.Action
toChatAction msg = defMessage & #actionChat .~ chat
  where
    chat =
        defMessage
            & #message -- cut the message because there is a limit
            .~ Data.Text.take 128 msg -- TODO: investigate exact limit

toAction :: Action -> A.Action
toAction (PointCommand ability us target) =
    defMessage
        & #actionRaw
        .~ attackRaw
  where
    attackRaw =
        defMessage
            & #unitCommand
            .~ attactCommand
    attactCommand =
        defMessage
            & #abilityId
            .~ fromIntegral (fromEnum ability)
            & #targetWorldSpacePos
            .~ toPoint2D target
            & #unitTags
            .~ [u ^. #tag | u <- us]
toAction (UnitCommand ability us target) =
    defMessage
        & #actionRaw
        .~ attackRaw
  where
    attackRaw =
        defMessage
            & #unitCommand
            .~ attactCommand
    attactCommand =
        defMessage
            & #abilityId
            .~ fromIntegral (fromEnum ability)
            & #targetUnitTag
            .~ (target ^. #tag)
            & #unitTags
            .~ [u ^. #tag | u <- us]
toAction (SelfCommand ability us) =
    defMessage
        & #actionRaw
        .~ attackRaw
  where
    attackRaw =
        defMessage
            & #unitCommand
            .~ attactCommand
    attactCommand =
        defMessage
            & #abilityId
            .~ fromIntegral (fromEnum ability)
            & #unitTags
            .~ [u ^. #tag | u <- us]