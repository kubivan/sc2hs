
module Squad.State where

import Actions (UnitTag)
import Footprint (Footprint)
import SC2.Grid (Region, RegionId)
import SC2.Grid.TilePos (TilePos)

type SquadFormation = (TilePos, Footprint)

data FSForming
    = FSFormingUnplaced
    | FSFormingPlaced SquadFormation

data FSExploreRegion = FSExploreRegion RegionId Region

data FSEngage
    = FSEngageFar UnitTag
    | FSEngageClose UnitTag

data SquadState
    = SSIdle
    | SSForming FSForming
    | SSExploreRegion FSExploreRegion
    | SSEngage FSEngage
    | SSRetreat (Maybe TilePos)

data UpdateResult
    = Continue SquadState
    | Transition SquadState
