
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Agent
  ( Agent (..),
    StepPlan (..),
    StaticInfo (..),
    AgentDynamicState(..),
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
import Data.List (foldl')
import Data.ProtoLens (defMessage)
import Data.Text (Text, pack)
import Debug.Trace
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
import Data.Kind (Type)
--type Observation = A.Observation

type UnitAbilities = HashMap.HashMap UnitTypeId [AbilityId.AbilityId]

type UnitTraits = HashMap.HashMap UnitTypeId A.UnitTypeData

data StepPlan = StepPlan
  {
    botCommands :: [Action]
  , botChat  :: [Text]
  , botDebug :: [DebugCommand]
  }

obsApplyAction :: Action -> Observation -> Observation
obsApplyAction a = addOrder (unit ^. #tag) ability
  where
    unit = getExecutor a
    ability = getCmd a

command :: AgentDynamicState d => [Action] -> StepMonad d ()
command acts = do
   when (not. null $ acts) $ trace ("command: " ++ (show $ getCmd <$> acts)) (return ())
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

agentObs :: AgentDynamicState dyn => StepMonad dyn Observation
agentObs = agentGet <&> getObs

agentStatic :: StepMonad dyn StaticInfo
agentStatic = agentAsk <&> fst

agentGet :: StepMonad dyn dyn
agentGet = lift get

agentPut :: dyn -> StepMonad dyn ()
agentPut = lift . put

--agentDsUpdate :: Observation -> Grid -> StepMonad dyn

runStep :: (Agent a d, AgentDynamicState d) => StaticInfo -> UnitAbilities -> d -> StepMonad d a -> (a, StepPlan, d)
--runStep staticInfo dynamicState stepMonad = runReaderT (runStateT (runWriterT stepMonad) dynamicState) staticInfo
runStep staticInfo abilities dynamicState stepMonad =
 let writerRes = runWriterT stepMonad
     stateRes = runStateT writerRes dynamicState
     ((a, stepPlan), dyn') = runReader stateRes (staticInfo, abilities)
 in (a, stepPlan, dyn')
class (AgentDynamicState d) => Agent a d | a -> d where
    type DynamicState a :: Type

    agentRace :: a -> C.Race
    agentStep :: a -> StepMonad d a

    makeDynamicState :: a -> Observation -> Grid -> d

type StepMonad d r = WriterT StepPlan (StateT d (Reader (StaticInfo, UnitAbilities))) r
type MaybeStepMonad d a = MaybeT (WriterT StepPlan (StateT d (Reader (StaticInfo, UnitAbilities)))) a