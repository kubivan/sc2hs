{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleContexts #-}

module Actions (Action(..), toAction, UnitTag) where

import Data.Text
import Lens.Micro ( (&), (.~), (&), (.~), (^.) )

import qualified Proto.S2clientprotocol.Sc2api as A
import Proto.S2clientprotocol.Raw as R
import Proto.S2clientprotocol.Sc2api_Fields as A
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()

import AbilityId

import Utils
--import Proto.S2clientprotocol.Raw_Fields (unitCommand, abilityId)
--import qualified GHC.Word
import GHC.Word (Word64)

type UnitTag = Word64

--data Action = Chat Text | Attack GHC.Word.Word64 Pointable
data Action =
   Chat Text 
  | forall a. Pointable a => UnitCommand AbilityId UnitTag a

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