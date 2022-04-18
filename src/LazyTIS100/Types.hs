{-# LANGUAGE TupleSections, LambdaCase, GeneralizedNewtypeDeriving, DeriveGeneric, DeriveFunctor, DeriveTraversable,
    ScopedTypeVariables #-}
module LazyTIS100.Types
    ( Port (..),
      JumpCondition (..),
      InstructionSource (..),
      InstructionTarget (..),
      Instruction (..),
      NodeMode (..),
      NodeState (..),
      Node (..),
      NodeIndex,
      NodeProgram,
      Cpu,
      defaultOutputNodeCapacity,
      initialNodeState,
      emptyComputeNode,
      initWithPrograms,
      getAccs,
      getAccsA,
      mapNodesToList,
      mapComputeNodesStateToList,
      getModes,
      getLastPorts,
      instructionSource,
      instructionTarget,
      printCpu
    ) where

import Control.Monad (forM, forM_, void, when)

import qualified Data.Array as A
import Data.Array (Array, (//))
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Either (either)
import qualified Data.Foldable
import Data.Maybe (catMaybes)
import Data.List (transpose)
import qualified Data.Sequence as Seq
import Debug.Trace
import Generic.Data (Generic, Generic1, gfmap)
import Generic.Functor (gbimap, gbifoldMap, gbitraverse)

data Port = UP | LEFT | RIGHT | DOWN | ANY | LAST
    deriving (Eq, Ord, Generic, Enum, Bounded, Show, Read)

data JumpCondition = JEZ | JNZ | JGZ | JLZ
    deriving (Eq, Ord, Generic, Enum, Bounded, Show, Read)
data InstructionSource n = SPort Port | SImmediate n | SAcc | SNil
    deriving (Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable, Read)
data InstructionTarget = TPort Port | TAcc | TNil
    deriving (Eq, Ord, Generic, Read)
data Instruction l n
    = JMP l | J JumpCondition l | JRO (InstructionSource n)
    | MOV (InstructionSource n) InstructionTarget
    | ADD (InstructionSource n) | SUB (InstructionSource n) | NEG
    | SWP | SAV | NOP
    --NOTE HCF is not reliable in this implementation because of lazy evaluation.
    | HCF
    deriving (Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable, Show, Read)

type NodeProgram l n = Array Int (Instruction l n)
data NodeMode n = IDLE | RUN | FINISHED | ONFIRE | READ Port | HasRead n | WRITE Port n | HasWritten
    deriving (Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable, Show, Read)
data NodeState n = NodeState { acc :: !n, bak :: !n, lastPort :: !Port, pc :: !Int, mode :: NodeMode n }
    deriving (Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable, Show, Read)
data Node l n = BrokenNode | InputNode [n]
    | OutputNode { outputNodeCapacity :: Int, outputNodeExpected :: Seq.Seq n, outputNodeActual :: Seq.Seq n }
    | ComputeNode (NodeProgram l n) (NodeState n)
    deriving (Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable, Show, Read)
type NodeIndex = (Int, Int)
type Cpu l n = Array NodeIndex (Node l n)

instance Show n => Show (InstructionSource n) where
    show (SPort port) = show port
    show (SImmediate x) = show x
    show SAcc = "ACC"
    show SNil = "NIL"

instance Show InstructionTarget where
    show (TPort port) = show port
    show TAcc = "ACC"
    show TNil = "NIL"

instance Bifunctor Instruction where
    bimap = gbimap

instance Bifoldable Instruction where
    bifoldMap = gbifoldMap

instance Bitraversable Instruction where
    bitraverse = gbitraverse

defaultOutputNodeCapacity :: Integral n => n
defaultOutputNodeCapacity = 39

initialNodeState :: Integral n => NodeState n
initialNodeState = NodeState 0 0 LAST 0 IDLE

emptyComputeNode :: Integral n => Node l n
emptyComputeNode = ComputeNode (A.listArray (0,-1) []) initialNodeState

initWithPrograms :: NodeState n -> Cpu l n -> [[Instruction l n]] -> Either String (Cpu l n)
initWithPrograms initialNodeState layout progs = go progs (A.elems layout) []
    where
        go (p : ps) (ComputeNode _ state : xs) acc = go ps xs (ComputeNode (A.listArray (0, length p - 1) p) initialNodeState : acc)
        go []       (ComputeNode _ state : xs) acc = Left "not enough programs"
        go ps       (node                : xs) acc = go ps xs (node : acc)
        go (_ : _)  []                         acc = Left "too many programs"
        go []       []                         acc = Right . A.listArray (A.bounds layout) . reverse $ acc

getAcc (ComputeNode _ state) = acc state
getAcc _                     = fromInteger 0

getAccsA :: Num n => Cpu l n -> Array NodeIndex n
getAccsA = fmap getAcc

mapNodesToList :: (Node l n -> a) -> Cpu l n -> [[a]]
mapNodesToList f cpu = map (\y -> map (f2 y) [x0..x1]) [y0..y1]
    where
        ((y0, x0), (y1, x1)) = A.bounds cpu
        f2 y x = f (cpu A.! (y, x))

mapComputeNodesStateToList :: a -> (NodeState n -> a) -> Cpu l n -> [[a]]
mapComputeNodesStateToList fallback f = mapNodesToList (\x -> case x of { ComputeNode _ state -> f state; _ -> fallback })

getAccs :: Num n => Cpu l n -> [[n]]
getAccs = mapComputeNodesStateToList (fromInteger 0) acc

getModes :: Cpu l n -> [[NodeMode n]]
getModes = mapComputeNodesStateToList FINISHED mode

getLastPorts :: Cpu l n -> [[Port]]
getLastPorts = mapComputeNodesStateToList LAST lastPort

instructionSource :: Instruction l n -> Maybe (InstructionSource n)
instructionSource (JRO s) = Just s
instructionSource (MOV s _) = Just s
instructionSource (ADD s) = Just s
instructionSource (SUB s) = Just s
instructionSource _ = Nothing

instructionTarget :: Instruction l n -> Maybe InstructionTarget
instructionTarget (MOV _ t) = Just t
instructionTarget _ = Nothing


padLine line = take 22 $ line ++ repeat ' '

nodeToStrings :: Node Int Int -> [String]
nodeToStrings BrokenNode = replicate 15 (replicate 18 '.' ++ "    ")
nodeToStrings (InputNode xs) = (++ [padLine "  \\/"]) $ take 14 $ reverse (map formatItem xs) ++ repeat (replicate 18 '-' ++ "    ")
    where formatItem x = padLine $ show x
nodeToStrings (OutputNode _ xs _) = ([padLine "  \\/"] ++) $ take 14 $ map formatItem (Data.Foldable.toList xs) ++ repeat (replicate 18 '-' ++ "    ")
    where formatItem x = padLine $ show x
nodeToStrings (ComputeNode prog NodeState {mode=FINISHED}) | A.bounds prog == (0, -1) =
    [padLine "---"] ++ replicate 14 (replicate 18 ' ' ++ "    ")
nodeToStrings (ComputeNode prog state) = take 15 $ map formatItem (A.assocs prog) ++ showState ++ repeat (replicate 18 ' ' ++ "    ")
    where
        formatItem (addr, inst) = padLine $ (if addr == pc state then (">"++) else (" "++)) $ show inst
        showState = map padLine ["", "#A=" ++ show (acc state), "#B=" ++ show (bak state)
            , "#" ++ show (mode state), "#L=" ++ show (lastPort state) ]

printCpu :: Cpu Int Int -> IO ()
printCpu cpu = do
    let ((y0, x0), (y1, x1)) = A.bounds cpu
    forM_ [y0..y1] $ \y -> do
        let lines = transpose $ map (nodeToStrings . (cpu A.!) . (y,)) [x0..x1]
        forM_ [x0..x1] $ \x ->
            putStr $ take (18+4) $ (show (x, y)) ++ repeat ' '
        putStrLn ""
        forM_ lines $ \lineParts -> putStrLn (concat lineParts)
