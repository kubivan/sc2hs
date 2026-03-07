
module Squad.State where

import Actions (UnitTag)
import Footprint (Footprint)
import SC2.Grid (Region, RegionId)
import SC2.Grid.TilePos (TilePos)

type SquadFormation = (TilePos, Footprint)

data FSIdle = FSIdle

data FSForming
    = FSFormingUnplaced
    | FSFormingPlaced SquadFormation

data FSExploreRegion = FSExploreRegion RegionId Region

data FSEngage
    = FSEngageFar UnitTag
    | FSEngageClose UnitTag

newtype FSRetreat = FSRetreat TilePos

data SquadState
    = SSIdle FSIdle
    | SSForming FSForming
    | SSExploreRegion FSExploreRegion
    | SSEngage FSEngage
    | SSRetreat FSRetreat