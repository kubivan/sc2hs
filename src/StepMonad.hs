{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module StepMonad (
    StepPlan (..),
    StaticInfo (..),
    AgentDynamicState (..),
    StepMonad,
    MaybeStepMonad,
    runStepM,
    agentChat,
    agentAsk,
    agentStatic,
    agentGet,
    agentObs,
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
import Grid.Grid
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
import Data.HashMap.Strict qualified as HashMap
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (pack)
import Lens.Micro ((^.))

type UnitTraits = HashMap.HashMap UnitTypeId UnitTypeData

obsApplyAction :: Action -> Observation -> Observation
obsApplyAction a obs = foldl (\obsAcc u -> addOrder (u ^. #tag) ability obsAcc) obs units
  where
    units = getExecutors a
    ability = getCmd a

command :: (AgentDynamicState d) => [Action] -> StepMonad d ()
command acts = do
    -- unless (null acts) $ trace ("command: " ++ (show $ getCmd <$> acts)) (return ())
    dyn <- agentGet

    let obs' = foldl' (flip obsApplyAction) (getObs dyn) acts
    put $ setObs obs' dyn

    tell (StepPlan acts [] [])

debug :: [DebugCommand] -> StepMonad dyn ()
debug acts = tell (StepPlan [] [] acts)

debugText :: String -> Point -> StepMonad dyn ()
debugText text p = debug [DebugText (pack text) p]

debugTexts :: [(String, Point)] -> StepMonad dyn ()
debugTexts = mapM_ (uncurry debugText)

agentChat :: String -> StepMonad dyn ()
agentChat msg = tell (StepPlan [] [pack msg] [])

data StaticInfo = StaticInfo
    { gameInfo :: ResponseGameInfo
    , playerInfo :: PlayerInfo
    , unitTraits :: UnitTraits
    , heightMap :: Grid
    , expandsPos :: [TilePos]
    , enemyStartLocation :: TilePos
    , regionGraph :: !(Map.Map RegionId (Set.Set RegionId))
    , regionLookup :: !(Map.Map TilePos RegionId)
    , siRegions :: !(Map.Map RegionId Region)
    }

class AgentDynamicState dyn where
    getObs :: dyn -> Observation
    getGrid :: dyn -> Grid

    setObs :: Observation -> dyn -> dyn
    setGrid :: Grid -> dyn -> dyn

    dsUpdate :: Observation -> Grid -> dyn -> dyn

agentAsk :: StepMonad dyn (StaticInfo, UnitAbilities)
agentAsk = lift $ lift ask

agentAbilities :: StepMonad dyn UnitAbilities
agentAbilities = agentAsk <&> snd

agentObs :: (AgentDynamicState dyn) => StepMonad dyn Observation
agentObs = agentGet <&> getObs

agentStatic :: StepMonad dyn StaticInfo
agentStatic = agentAsk <&> fst

agentGet :: StepMonad dyn dyn
agentGet = lift get

agentPut :: dyn -> StepMonad dyn ()
agentPut = lift . put

runStepM :: StaticInfo -> UnitAbilities -> d -> StepMonad d a -> (a, StepPlan, d)
runStepM staticInfo abilities dynamicState stepMonad =
    let writerRes = runWriterT stepMonad
        stateRes = runStateT writerRes dynamicState
        ((a, stepPlan), dyn') = runReader stateRes (staticInfo, abilities)
     in (a, stepPlan, dyn')

type StepMonad d r = WriterT StepPlan (StateT d (Reader (StaticInfo, UnitAbilities))) r
type MaybeStepMonad d a = MaybeT (WriterT StepPlan (StateT d (Reader (StaticInfo, UnitAbilities)))) a