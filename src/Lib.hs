{-# LANGUAGE TupleSections, LambdaCase, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts,
    ScopedTypeVariables #-}
module Lib
    ( Port (..),
      JumpCondition (..),
      InstructionSource (..),
      InstructionTarget (..),
      Instruction (..),
      NodeMode (..),
      NodeState (..),
      Node (..),
      NodeProgram,
      Cpu,
      initialNodeState,
      emptyComputeNode,
      initWithPrograms,
      getAccs,
      getAccsA,
      mapNodesToList,
      mapComputeNodesStateToList,
      getModes,
      getLastPorts,
      portOrderReadAny,
      step,
      stepN,
      steps,
      printCpu
    ) where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, void, when)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError, catchError)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import qualified Control.Monad.Reader
import Control.Monad.State (MonadState, State, StateT, runStateT, runState)
import qualified Control.Monad.State

import qualified Data.Array as A
import Data.Array (Array, (//))
import Data.Either (either)
import Data.Maybe (catMaybes)
import Data.List (transpose)
import Debug.Trace

import LazyTIS100.Types
import LazyTIS100.Parser
import LazyTIS100.EvalEager
