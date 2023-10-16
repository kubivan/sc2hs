{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleContexts #-}

module Actions (Action(..), toAction, UnitTag, toDebug, DebugCommand(..)) where

import Data.Text
import Lens.Micro ( (&), (.~), (&), (.~), (^.) )

import qualified Proto.S2clientprotocol.Sc2api as A
import qualified Proto.S2clientprotocol.Debug as D
import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Raw as R
import Proto.S2clientprotocol.Sc2api_Fields as A
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()

import AbilityId

import Utils
--import Proto.S2clientprotocol.Raw_Fields (unitCommand, abilityId)
--import qualified GHC.Word
import GHC.Word (Word64)
import qualified Proto.S2clientprotocol.Debug as D

type UnitTag = Word64

--data Action = Chat Text | Attack GHC.Word.Word64 Pointable
data Action =
   Chat Text 
  | forall a. Pointable a => UnitCommand AbilityId UnitTag a

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
toDebug (DebugText t p) = defMessage & #draw .~ drawMsg where
  drawMsg :: D.DebugDraw
  drawMsg = defMessage & #text .~ [textMsg]
  textMsg :: D.DebugText
  textMsg = defMessage & #text .~ t & #worldPos .~ p

toAction :: Action -> A.Action
toAction (Chat msg) = defMessage & #actionChat .~ chat
  where
    chat = defMessage & #message .~ msg

toAction (UnitCommand ability u target) = defMessage
  & #actionRaw .~ attackRaw
  where
    attackRaw = defMessage
      & #unitCommand .~ attactCommand
    attactCommand = defMessage
      & #abilityId .~ fromIntegral (fromEnum ability)
      & #targetWorldSpacePos .~ to2D target
      & #unitTags .~ [u]