
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Agent
  ( Agent (..),
    StepPlan (..),
    StaticInfo (..),
    DynamicState,
    StepMonad,
    MaybeStepMonad,
    runStep,
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

import AbilityId qualified
import Actions (Action, DebugCommand(..), getCmd, getExecutor)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Strict
import Data.Functor
import Data.HashMap.Strict qualified as HashMap
import Data.ProtoLens (defMessage)
import Data.Text (Text, pack)
import GHC.Word qualified
import Grid
import Lens.Micro ((^.), (.~), (&))
import Proto.S2clientprotocol.Common as C
import Proto.S2clientprotocol.Common_Fields as C
import Proto.S2clientprotocol.Data qualified as A
import Proto.S2clientprotocol.Sc2api qualified as A
import Proto.S2clientprotocol.Sc2api_Fields qualified as A
import UnitTypeId
--import Utils (Pointable, dbg)
import Utils

import Observation
--type Observation = A.Observation

type UnitAbilities = HashMap.HashMap UnitTypeId [AbilityId.AbilityId]

type UnitTraits = HashMap.HashMap UnitTypeId A.UnitTypeData

data StepPlan = StepPlan
  {
    botCommands :: [Action]
  , botChat  :: [Text]
  , botDebug :: [DebugCommand]
  }

command :: [Action] -> StepMonad ()
command acts = do
   tell (StepPlan acts [] [])

debug :: [DebugCommand] -> StepMonad ()
debug acts = tell (StepPlan [] [] acts)

debugText :: String -> C.Point -> StepMonad ()
debugText text p = debug [DebugText (pack text) p]

debugTexts :: [(String, C.Point)] -> StepMonad ()
debugTexts = mapM_ (uncurry debugText)

agentChat :: String -> StepMonad ()
agentChat msg = tell (StepPlan [] [pack msg] [])

instance Semigroup StepPlan where
  (<>) (StepPlan as1 cs1 ds1) (StepPlan as2 cs2 ds2) = StepPlan (as1 <> as2) (cs1 <> cs2) (ds1 <> ds2)

instance Monoid StepPlan where
    mempty = StepPlan [] [] []

data StaticInfo = StaticInfo
  { gameInfo :: A.ResponseGameInfo,
    playerInfo :: A.PlayerInfo,
    unitTraits :: UnitTraits,
    heightMap :: Grid,
    expandsPos :: [TilePos]
  }

type DynamicState = (Observation, Grid)

type StepMonad a = WriterT StepPlan (StateT DynamicState (Reader (StaticInfo, UnitAbilities))) a
type MaybeStepMonad a = MaybeT (WriterT StepPlan (StateT DynamicState (Reader (StaticInfo, UnitAbilities)))) a

agentAsk :: StepMonad (StaticInfo, UnitAbilities)
agentAsk = lift $ lift ask

agentAbilities :: StepMonad UnitAbilities
agentAbilities = agentAsk <&> snd

agentObs :: StepMonad Observation
agentObs = agentGet <&> fst

agentStatic :: StepMonad StaticInfo
agentStatic = agentAsk <&> fst

agentGet :: StepMonad DynamicState
agentGet = lift get

agentPut :: DynamicState -> StepMonad ()
agentPut x@(_, grid) = lift $ put x

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
