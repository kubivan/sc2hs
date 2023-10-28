
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Agent(Agent(..), StepPlan(..), StaticInfo(..), DynamicState, StepMonad, MaybeStepMonad, runStep, agentAsk, agentStatic, agentGet, agentAbilities, agentPut, UnitAbilities, UnitTraits, Observation, debug, command, toEnum', fromEnum') where

import Actions

import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import qualified Proto.S2clientprotocol.Sc2api as A
import qualified Proto.S2clientprotocol.Sc2api_Fields as A

import Control.Monad.Writer.Strict
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.HashMap.Strict as HashMap

import qualified AbilityId
import UnitTypeId
import qualified Proto.S2clientprotocol.Data as A
import Grid
import Control.Monad.Identity (Identity(..))
import Control.Monad.Trans.Maybe
import GHC.Word qualified
import Data.Functor
import Utils(dbg)
import Grid (gridToString)

type Observation = A.Observation

type UnitAbilities = HashMap.HashMap UnitTypeId [AbilityId.AbilityId]

type UnitTraits = HashMap.HashMap UnitTypeId A.UnitTypeData

toEnum' :: Enum e => GHC.Word.Word32 -> e
toEnum' = toEnum . fromIntegral

fromEnum' :: Enum e => e -> GHC.Word.Word32
fromEnum' = fromIntegral . fromEnum

data StepPlan = StepPlan
  {
    botCommands :: [Action]
  , botDebug :: [DebugCommand]
  }

command :: [Action] -> StepMonad ()
command acts = tell (StepPlan acts [])

debug :: [DebugCommand] -> StepMonad ()
debug acts = tell (StepPlan [] acts)

instance Semigroup StepPlan where
  (<>) (StepPlan as1 ds1) (StepPlan as2 ds2) = StepPlan (as1 <> as2) (ds1 <> ds2)

instance Monoid StepPlan where
    mempty = StepPlan [] []

data StaticInfo = StaticInfo { gameInfo :: A.ResponseGameInfo, playerInfo :: A.PlayerInfo, unitTraits :: UnitTraits}

type DynamicState = (Observation, Grid)

type StepMonad a = WriterT StepPlan (StateT DynamicState (Reader (StaticInfo, UnitAbilities))) a
type MaybeStepMonad a = MaybeT (WriterT StepPlan (StateT DynamicState (Reader (StaticInfo, UnitAbilities)))) a

agentAsk :: StepMonad (StaticInfo, UnitAbilities)
agentAsk = lift $ lift ask

agentAbilities :: StepMonad UnitAbilities
agentAbilities = agentAsk <&> snd

agentStatic :: StepMonad StaticInfo
agentStatic = agentAsk <&> fst

agentGet :: StepMonad DynamicState
agentGet = lift get

agentPut :: DynamicState -> StepMonad ()
agentPut x@(_, grid) = lift $ put x --`Utils.dbg` ("agentPut: \n" ++ gridToString grid)

runStep :: StaticInfo -> UnitAbilities -> DynamicState -> StepMonad a -> (a, StepPlan, Grid)
--runStep staticInfo dynamicState stepMonad = runReaderT (runStateT (runWriterT stepMonad) dynamicState) staticInfo
runStep staticInfo abilities dynamicState stepMonad =
  let writerRes = runWriterT stepMonad
      stateRes = runStateT writerRes dynamicState
      ((a, stepPlan), (obs', grid')) = runReader stateRes (staticInfo, abilities)
  in (a, stepPlan, grid')

class Agent a where
    agentRace :: a -> C.Race
    agentStep :: a -> StepMonad a
    agentDebug :: a -> IO ()