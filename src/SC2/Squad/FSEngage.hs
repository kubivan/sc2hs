
module SC2.Squad.FSEngage where

import SC2.Utils
import Actions
import Units
import SC2.Grid
import SC2.Squad.Class
import SC2.Squad.Squad
import SC2.Squad.Behavior
import SC2.Geometry
import StepMonad
import StepMonadUtils
import Actions (Action (..), UnitTag)
import SC2.Ids.AbilityId
import SC2.Ids.UnitTypeId
import Observation

import Control.Monad (void)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.Char (isDigit)
import Data.ProtoLens (defMessage)
import Data.Maybe (fromJust)
import Conduit

import Data.Set qualified as Set
import Lens.Micro
import Lens.Micro.Extras (view)

import Debug.Trace
import Footprint

import Proto.S2clientprotocol.Common ( Point, Point2D )
import Proto.S2clientprotocol.Common_Fields ( x, y, z )
import Observation (getUnit)
import SC2.Geometry (toPoint2D)
import SC2.Squad.Class (HasArmy)
import Proto.S2clientprotocol.Data (UnitTypeData)

data FSEngage = FSSeek UnitTag

--unitSeek :: HasArmy d => Unit -> Unit -> StepMonad d ()

enemyStr :: Unit -> String
enemyStr u = show (u ^. #tag) ++ ", " ++ show ((toEnum' (u ^. #unitType)) :: UnitTypeId) ++ " " ++ show (tilePos (u ^. #pos))

unitSeek :: Unit -> Unit -> Point2D
unitSeek unit enemy =
    let enemyPos2D = toPoint2D $ enemy ^. #pos
        speed = 4.13 -- TODO: remove hardcoded stalkers value
        desiredVelocity = vecScale speed $ vecNormalize $ enemyPos2D - toPoint2D (unit ^. #pos)
        unitVelocity = unitVelocityVec unit
    in trace (show (unit ^. #tag) ++ " egaging: " ++ enemyStr enemy ++ ": " ++ show (desiredVelocity - unitVelocity))
        desiredVelocity - unitVelocity


instance SquadFS FSEngage where

    fsStep squad (FSSeek enemyTag) = do
        ds <- agentGet
        obs <- agentObs
        traceM ("[step] FSSeek " ++ show (squadId squad))
        let unitByTag t = HashMap.lookup t (getUnitMap ds)
            enemy = fromJust $ getUnit obs enemyTag
            -- TODO: it shouldn't happen: updateArmy had to remove dead units from squads
            units = catMaybes $ [unitByTag t | t <- squadUnits squad]

        debugUnit enemy
        mapM_ (\u -> debugUnitVec u (unitSeek u enemy)) units

    fsUpdate squad st@(FSSeek enemyTag) =
        do
            ds <- agentGet
            obs <- agentObs
            let unitByTag t = HashMap.lookup t (getUnitMap ds)
            case getUnit obs enemyTag of
                Nothing -> do
                    traceM  ("no more unit with tag " ++ show enemyTag)
                    return (True, st)

                _ -> return (False, FSSeek enemyTag)

    fsOnEnter s _ = traceM $ "[enter] FSEngage " ++ show (squadId s)
    fsOnExit  s _ = traceM $ "[exit] FSEngage " ++ show (squadId s)

-- data FSAttack = FSAttack Target
-- data FSEvade = FSEvade