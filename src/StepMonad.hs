{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
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

import Agent
import AbilityId qualified
import Actions (Action, DebugCommand (..), getCmd, getExecutors)
import Control.Monad
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Strict
import Data.Functor
import Data.HashMap.Strict qualified as HashMap
import Data.List (foldl')
import Data.Map qualified as Map
import Data.ProtoLens (defMessage)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Debug.Trace
import GHC.Word qualified
import Grid.Grid
import Lens.Micro ((&), (.~), (^.))
import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import Proto.S2clientprotocol.Data qualified as A
import Proto.S2clientprotocol.Sc2api qualified as A
import Proto.S2clientprotocol.Sc2api_Fields qualified as A
import UnitTypeId

import Utils

import Data.Kind (Type)
import Observation
import UnitAbilities


type UnitTraits = HashMap.HashMap UnitTypeId A.UnitTypeData

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

debugText :: String -> C.Point -> StepMonad dyn ()
debugText text p = debug [DebugText (pack text) p]

debugTexts :: [(String, C.Point)] -> StepMonad dyn ()
debugTexts = mapM_ (uncurry debugText)

agentChat :: String -> StepMonad dyn ()
agentChat msg = tell (StepPlan [] [pack msg] [])

data StaticInfo = StaticInfo
    { gameInfo :: A.ResponseGameInfo
    , playerInfo :: A.PlayerInfo
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

-- runStepM :: (Agent a, AgentDynamicState d) => StaticInfo -> UnitAbilities -> d -> StepMonad d a -> (a, StepPlan, d)
runStepM staticInfo abilities dynamicState stepMonad =
    let writerRes = runWriterT stepMonad
        stateRes = runStateT writerRes dynamicState
        ((a, stepPlan), dyn') = runReader stateRes (staticInfo, abilities)
     in (a, stepPlan, dyn')

type StepMonad d r = WriterT StepPlan (StateT d (Reader (StaticInfo, UnitAbilities))) r
type MaybeStepMonad d a = MaybeT (WriterT StepPlan (StateT d (Reader (StaticInfo, UnitAbilities)))) a