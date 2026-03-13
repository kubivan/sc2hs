module Intent where

import Actions (Action, UnitTag, getCmd, getExecutors)
import Observation (Cost)
import SC2.Grid (TilePos)
import SC2.Ids.AbilityId (AbilityId)
import SC2.Ids.UnitTypeId (UnitTypeId)

import Data.HashMap.Strict (HashMap)
import Data.Word (Word32)
import Lens.Micro ((^.))

data BuildIntentState
  = IntentIssued
  | IntentConfirmed
  | IntentRolledBack
  deriving (Eq, Show)

data BuildRollbackReason
  = RollbackActionError
  | RollbackExecutorMissing
  | RollbackInterrupted
  deriving (Eq, Show)

data GhostMarkRef = GhostMarkRef
  { gmrUnitType :: UnitTypeId
  , gmrPos :: TilePos
  }
  deriving (Eq, Show)

data IntentAction
  = IntentReserveAction Cost
  | IntentBuildAction IntentBuildCommand
  deriving (Show)

data IntentBuildTarget
  = IntentBuildNoTarget
  | IntentBuildAt TilePos
  deriving (Eq, Show)

data IntentBuildCommand = IntentBuildCommand
  { ibcExecutor :: UnitTag
  , ibcAbility :: AbilityId
  , ibcTarget :: IntentBuildTarget
  }
  deriving (Eq, Show)

type BuildIntentId = (UnitTag, AbilityId)

data BuildIntent = BuildIntent
  { biId :: BuildIntentId
  , biExecutor :: UnitTag
  , biAbility :: AbilityId
  , biActions :: [IntentAction]
  , biRollbackStack :: [IntentAction]
  , biUnitType :: UnitTypeId
  , biReservedCost :: Cost
  , biGhostMarks :: [GhostMarkRef]
  , biIssuedAtFrame :: Word32
  , biState :: BuildIntentState
  , biRollbackReason :: Maybe BuildRollbackReason
  }
  deriving (Show)

type BuildIntentStore = HashMap BuildIntentId BuildIntent

type IntentOrder = [BuildIntent]

actionIntentId :: Action -> Maybe BuildIntentId
actionIntentId action = case getExecutors action of
  [] -> Nothing
  (executor : _) -> Just (executor ^. #tag, getCmd action)
