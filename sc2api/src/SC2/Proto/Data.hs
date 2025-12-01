{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module SC2.Proto.Data (
    -- -- reexporting sc2-proto types
    -- A.Race(..),
    -- A.Request,
    -- A.PlayerResult,
    -- A.ResponseData,
    -- A.ResponseGameInfo,
    -- A.ResponseObservation,
    -- A.Alliance(..),
    -- A.Point2D,
    -- A.AbilityData, -- TODO: we have own type, need to resolve
    -- A.UnitTypeData, -- TODO: we have own type
    -- A.Unit,
    -- A.Point,
    -- A.abilityId,
    -- A.Action,
    -- A.Observation,
    module C,
    module R,
    module S,
    module D,
    module SF,

    Map (LocalMap),
) where

import Data.ByteString qualified as BS
import Data.ProtoLens.Labels ()
import Data.Text

-- import Proto.S2clientprotocol.Common as A
-- import Proto.S2clientprotocol.Raw as A
-- import Proto.S2clientprotocol.Sc2api as A
-- import Proto.S2clientprotocol.Sc2api_Fields as A
-- import Proto.S2clientprotocol.Data as A
import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Raw as R
import Proto.S2clientprotocol.Raw_Fields as RF
import Proto.S2clientprotocol.Sc2api as S hiding (PlayerType(..)) -- hiding participant
import Proto.S2clientprotocol.Sc2api_Fields as SF
import Proto.S2clientprotocol.Data as D


data Map = LocalMap Text (Maybe BS.ByteString)