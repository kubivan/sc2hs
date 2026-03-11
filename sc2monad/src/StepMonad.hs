{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module StepMonad (
    StepPlan (..),
        AsyncStaticInfo (..),
    StaticInfo (..),
        siRegionGraph,
        siRegionLookup,
        siRegionsResolved,
        siRegionPathToEnemyResolved,
    StepMonad,
    HasObs(..),
    HasGrid(..),
    MaybeStepMonad,
    runStepM,
    agentChat,
    agentAsk,
    agentStatic,
    agentGet,
    agentObs,
    agentGrid,
    agentModify,
    agentModifyGrid,
    agentModifyObs,
    agentAbilities,
    agentPut,
    UnitAbilities,
    UnitTraits,
    debug,
    debugText,
    debugTexts,
    command,
)
where

import Actions (Action, DebugCommand (..), getCmd, getExecutors)
import Agent
import SC2.Grid
import SC2.TechTree
import Observation
import SC2.Ids.UnitTypeId
import SC2.Proto.Data (PlayerInfo, Point, ResponseGameInfo, UnitTypeData)
import UnitAbilities
import Utils

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Strict
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (pack)
import Lens.Micro ((^.), (%~), Lens')


obsApplyAction :: Action -> Observation -> Observation
obsApplyAction a obs = foldl (\obsAcc u -> addOrder (u ^. #tag) ability obsAcc) obs units
  where
    units = getExecutors a
    ability = getCmd a

command :: (HasObs d) => [Action] -> StepMonad d ()
command acts = do
    -- unless (null acts) $ trace ("command: " ++ (show $ getCmd <$> acts)) (return ())
    dyn <- agentGet

    agentModifyObs $ \obs -> foldl' (flip obsApplyAction) obs acts

    tell (StepPlan acts [] [])

debug :: [DebugCommand] -> StepMonad dyn ()
debug acts = tell (StepPlan [] [] acts)

debugText :: String -> Point -> StepMonad dyn ()
debugText text p = debug [DebugText (pack text) p]

debugTexts :: [(String, Point)] -> StepMonad dyn ()
debugTexts = mapM_ (uncurry debugText)

agentChat :: String -> StepMonad dyn ()
agentChat msg = tell (StepPlan [] [pack msg] [])

data AsyncStaticInfo = AsyncStaticInfo
    { asiRegionGraph :: !RegionGraph
    , asiRegionLookup :: !RegionLookup
    , asiRegions :: !(HashMap RegionId Region)
    , asiRegionPathToEnemy :: ![RegionId]
    }

type RegionLookup = HashMap TilePos RegionId

data StaticInfo = StaticInfo
    { gameInfo :: ResponseGameInfo
    , playerInfo :: PlayerInfo
    , unitTraits :: UnitTraits
    , heightMap :: Grid
    , expandsPos :: [TilePos]
    , startLocation :: TilePos
    , enemyStartLocation :: TilePos
    , siAsyncStaticInfo :: !(Maybe AsyncStaticInfo)
    }

siRegionGraph :: StaticInfo -> RegionGraph
siRegionGraph = maybe HashMap.empty asiRegionGraph . siAsyncStaticInfo

siRegionLookup :: StaticInfo -> RegionLookup
siRegionLookup = maybe HashMap.empty asiRegionLookup . siAsyncStaticInfo

siRegionsResolved :: StaticInfo -> Maybe (HashMap RegionId Region)
siRegionsResolved = fmap asiRegions . siAsyncStaticInfo

siRegionPathToEnemyResolved :: StaticInfo -> Maybe [RegionId]
siRegionPathToEnemyResolved = fmap asiRegionPathToEnemy . siAsyncStaticInfo

class HasObs s where
  obsL :: Lens' s Observation

class HasGrid s where
  gridL :: Lens' s Grid

agentAsk :: StepMonad dyn (StaticInfo, UnitAbilities)
agentAsk = lift $ lift ask

agentAbilities :: StepMonad dyn UnitAbilities
agentAbilities = agentAsk <&> snd

agentObs :: (HasObs dyn) => StepMonad dyn Observation
agentObs = agentGet <&> (^. obsL)

agentModifyObs :: (HasObs d) => (Observation -> Observation) -> StepMonad d ()
agentModifyObs f = agentModify (obsL %~ f)

agentGrid :: (HasGrid dyn) => StepMonad dyn Grid
agentGrid = agentGet <&> (^. gridL)

agentModifyGrid :: (HasGrid d) => (Grid -> Grid) -> StepMonad d ()
agentModifyGrid f = agentModify (gridL %~ f)

agentStatic :: StepMonad dyn StaticInfo
agentStatic = agentAsk <&> fst

agentGet :: StepMonad dyn dyn
agentGet = lift get

agentPut :: dyn -> StepMonad dyn ()
agentPut = lift . put

agentModify :: (d -> d) -> StepMonad d ()
agentModify f = agentGet >>= agentPut . f

runStepM :: StaticInfo -> UnitAbilities -> d -> StepMonad d a -> (a, StepPlan, d)
runStepM staticInfo abilities dynamicState stepMonad =
    let writerRes = runWriterT stepMonad
        stateRes = runStateT writerRes dynamicState
        ((a, stepPlan), dyn') = runReader stateRes (staticInfo, abilities)
     in (a, stepPlan, dyn')

type StepMonad d =
  WriterT StepPlan (StateT d (Reader (StaticInfo, UnitAbilities)))

type MaybeStepMonad d a = MaybeT (StepMonad d) a
