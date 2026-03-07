
module Squad.State where

import Actions (UnitTag)
import Footprint (Footprint)
import SC2.Grid (Region, RegionId)
import SC2.Grid.TilePos (TilePos)

type SquadFormation = (TilePos, Footprint)

data SquadState
    = SSIdle
    | SSForming (Maybe SquadFormation)
    | SSExploreRegion RegionId Region
    | SSEngageFar UnitTag
    | SSEngageClose UnitTag
    | SSRetreat TilePos