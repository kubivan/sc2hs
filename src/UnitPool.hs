{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}

module UnitPool(unitsSelf) where

import qualified Proto.S2clientprotocol.Sc2api as P
import Proto.S2clientprotocol.Raw as R
import Proto.S2clientprotocol.Sc2api_Fields as P
import Proto.S2clientprotocol.Raw_Fields as R

import Lens.Micro ( (&), (.~), (&), (.~), (^.) )

unitsSelf obs = filter (\u -> u ^. R.alliance == R.Self) (obs ^. (P.rawData . P.units))