{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Actions (Action(..), toAction) where

import Data.Text

import qualified Proto.S2clientprotocol.Sc2api as A
import Proto.S2clientprotocol.Sc2api_Fields as A
import Lens.Micro((&), (.~), (^.))
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()

data Action = Chat Text

toAction :: Action -> A.Action
toAction (Chat msg) = defMessage & #actionChat .~ chat
  where 
    chat = defMessage & #message .~ msg