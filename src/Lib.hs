{-# LANGUAGE TupleSections, LambdaCase #-}
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
      step,
      stepN,
      steps,
      printCpu
    ) where

import Control.Monad (forM, forM_)
import qualified Data.Array as A
import Data.Array (Array, (//))
import Data.Either (either)
import Data.Maybe (catMaybes)
import Data.List (transpose)
import Debug.Trace

data Port = UP | LEFT | RIGHT | DOWN | ANY | LAST
    deriving (Eq, Ord, Enum, Show, Read)

-- https://kk4ead.github.io/tis-100/
portOrderReadAny = [ LEFT, RIGHT, UP, DOWN ]

data JumpCondition = JEZ | JNZ | JGZ | JLZ
    deriving (Eq, Ord, Enum, Show, Read)
data InstructionSource n = SPort Port | SImmediate n | SAcc | SNil
    deriving (Eq, Ord, Read)
data InstructionTarget = TPort Port | TAcc | TNil
    deriving (Eq, Ord, Read)
data Instruction l n
    = JMP l | J JumpCondition l | JRO (InstructionSource n)
    | MOV (InstructionSource n) InstructionTarget
    | ADD (InstructionSource n) | SUB (InstructionSource n) | NEG
    | SWP | SAV | NOP
    --NOTE HCF is not reliable in this implementation because of lazy evaluation.
    | HCF
    deriving (Eq, Ord, Show, Read)

type NodeProgram l n = Array Int (Instruction l n)
data NodeMode n = IDLE | RUN | FINISHED | ONFIRE | READ Port | HasRead n | WRITE Port n | HasWritten
    deriving (Eq, Ord, Show, Read)
data NodeState n = NodeState { acc :: !n, bak :: !n, lastPort :: !Port, pc :: !Int, mode :: NodeMode n }
    deriving (Eq, Ord, Show, Read)
data Node l n = BrokenNode | InputNode [n] | OutputNode [n] | ComputeNode (NodeProgram l n) (NodeState n)
    deriving (Eq, Ord, Show, Read)
type Cpu l n = Array (Int, Int) (Node l n)

instance Show n => Show (InstructionSource n) where
    show (SPort port) = show port
    show (SImmediate x) = show x
    show SAcc = "ACC"
    show SNil = "NIL"

instance Show InstructionTarget where
    show (TPort port) = show port
    show TAcc = "ACC"
    show TNil = "NIL"

initialNodeState :: NodeState Int
initialNodeState = NodeState 0 0 LAST 0 IDLE

emptyComputeNode :: Node l Int
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

getAccsA :: Num n => Cpu l n -> Array (Int, Int) n
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

stepPrepareRead1 :: Instruction l n -> NodeState n -> Maybe (NodeState n)
stepPrepareRead1 inst state = case instructionSource inst of
    Just (SPort p) -> Just $ state { mode = READ p }
    _ -> Nothing

--FIXME merge with stepRead
stepPrepareRead :: Cpu l n -> Cpu l n
stepPrepareRead cpu = cpu // go (A.assocs cpu)
    where
        go [] = []
        go ((ix, ComputeNode prog state) : xs) =
            case prog `at` pc state of
                Nothing -> go xs
                Just inst ->
                    case stepPrepareRead1 inst state of
                        Nothing -> go xs
                        Just state' -> (ix, ComputeNode prog state') : go xs
        go (x : xs) = go xs

neighbourIndex :: (Int, Int) -> Port -> ((Int, Int), Port, Port)
neighbourIndex (y, x) LEFT  = ((y, x-1), LEFT,  RIGHT)
neighbourIndex (y, x) RIGHT = ((y, x+1), RIGHT, LEFT)
neighbourIndex (y, x) UP    = ((y-1, x), UP,    DOWN)
neighbourIndex (y, x) DOWN  = ((y+1, x), DOWN,  UP)
neighbourIndex (y, x) z     = ((y, x), z, z)  -- usually invalid but allowed special case for LAST

isValidNodeIndex :: Cpu l n -> (Int, Int) -> Bool
isValidNodeIndex cpu ix = A.inRange (A.bounds cpu) ix

neighbourIndicesForRead :: Cpu l n -> NodeState n -> (Int, Int) -> Port -> [((Int, Int), Port, Port)]
neighbourIndicesForRead cpu state myIndex port = map (neighbourIndex myIndex) $ applicableNeighbours port
    where
        applicableNeighbours ANY = portOrderReadAny
        applicableNeighbours LAST = [lastPort state]
        applicableNeighbours p = [p]

at :: A.Ix i => Array i e -> i -> Maybe e
arr `at` ix = if A.inRange (A.bounds arr) ix then Just (arr A.! ix) else Nothing

tryRead :: Num n => Cpu l n -> NodeState n -> (Int, Int) -> Port -> Maybe ([((Int, Int), Node l n)], Port, n)
tryRead cpu state ix port = go $ neighbourIndicesForRead cpu state ix port
    where
        -- special case: LAST used without preceding ANY is like NIL
        go ((ix', LAST, LAST) : xs)
            | ix == ix' = Just ([], LAST, fromInteger 0)
        go ((ix', myPort, theirPort) : xs) = case (cpu `at` ix') of
            Just (InputNode (v : vs)) ->
                Just ([(ix', InputNode vs)], myPort, v)
            Just (ComputeNode prog theirState@(NodeState {mode = WRITE ANY   v})) ->
                Just ([(ix', ComputeNode prog (theirState {mode = HasWritten, lastPort = theirPort}))], myPort, v)
            Just (ComputeNode prog theirState@(NodeState {mode = WRITE port' v})) | port' == theirPort ->
                Just ([(ix', ComputeNode prog (theirState {mode = HasWritten}))], myPort, v)
            _ -> go xs
        go [] = Nothing

stepRead :: Num n => Cpu l n -> Cpu l n
stepRead cpu = foldl go cpu (A.assocs cpu)
    where
        go :: Num n => Cpu l n -> ((Int, Int), Node l n) -> Cpu l n
        go cpu (ix, ComputeNode prog state@(NodeState {mode = READ port})) =
            seq cpu $ case tryRead cpu state ix port of
                Nothing -> cpu
                Just (updates, actualPort, value) -> cpu // ((ix, ComputeNode prog state {mode = HasRead value, lastPort = if port == ANY then actualPort else lastPort state}) : updates)
        go cpu (ix, OutputNode xs) =
            seq cpu $ case tryRead cpu (NodeState { acc = 0, bak = 0, lastPort = LAST, pc = 0, mode = READ UP }) ix UP of
                Nothing -> cpu
                Just (updates, actualPort, value) -> cpu // ((ix, OutputNode (value : xs)) : updates)
        go cpu _ = cpu

saturate :: Ord i => (i, i) -> i -> i
saturate (min, max) x
    | x < min = min
    | x > max = max
    | otherwise = x

stepRun :: Cpu Int Int -> (Bool, Cpu Int Int)
stepRun cpu = (isDone (map snd updates), cpu // updates)
    where
        updates = catMaybes (map go (A.assocs cpu))

        isDone :: [Node Int Int] -> Bool
        isDone [] = True
        isDone (ComputeNode _ NodeState { mode = ONFIRE } : xs) = True
        isDone (ComputeNode _ NodeState { mode = FINISHED } : xs) = isDone xs
        isDone (ComputeNode _ NodeState { mode = _ } : xs) = False
        isDone (_ : xs) = isDone xs

        go :: ((Int, Int), Node Int Int) -> Maybe ((Int, Int), Node Int Int)
        go (ix, ComputeNode prog state) = (ix,) . ComputeNode prog <$> case prog `at` pc state of
            Nothing -> Just $ state { mode = FINISHED}
            Just inst ->  go' prog state inst
        go _ = Nothing

        nextInstruction prog state
            | pc state + 1 > snd (A.bounds prog) = state { pc = 0, mode = RUN }
            | otherwise = state { pc = (pc state) + 1, mode = RUN }

        getSourceValue state (SPort _) = case mode state of
            HasRead x -> Just x
            _ -> Nothing
        getSourceValue state (SImmediate x) = Just x
        getSourceValue state SAcc = Just $ acc state
        getSourceValue state SNil = Just 0

        go' :: NodeProgram Int Int -> NodeState Int -> Instruction Int Int -> Maybe (NodeState Int)
        go' prog state@NodeState { mode = HasWritten } _ = Just $ nextInstruction prog state
        go' prog state (JMP lbl) | mode state /= FINISHED =
            let newPc = saturate (A.bounds prog) lbl in
            Just $ state { pc = newPc, mode = if newPc == pc state then FINISHED else RUN }
        go' prog state (J cond lbl) = let
            newPc = saturate (A.bounds prog) lbl
            doJump = case cond of
                JEZ -> acc state == 0
                JNZ -> acc state /= 0
                JGZ -> acc state > 0
                JLZ -> acc state < 0
            in if doJump
                then Just $ state { pc = newPc, mode = if newPc == pc state then FINISHED else RUN }
                else Just $ nextInstruction prog state
        go' prog state (JRO src) = getSourceValue state src >>= \value ->
            let newPc = saturate (A.bounds prog) (pc state + fromInteger (toInteger value)) in
            Just $ state { pc = newPc }
        go' prog NodeState { mode = WRITE _ _ } (MOV _ _) = Nothing  -- still waiting for value to be written
        go' prog state (MOV src tgt) = getSourceValue state src >>= \value ->
            case tgt of
                TPort port -> Just $ state { mode = WRITE port value }
                TAcc -> Just $ nextInstruction prog state { acc = value }
                TNil -> Just $ nextInstruction prog state
        go' prog state (ADD src) = getSourceValue state src >>= \value ->
            Just $ nextInstruction prog state { acc = acc state + value }
        go' prog state (SUB src) = getSourceValue state src >>= \value ->
            Just $ nextInstruction prog state { acc = acc state - value }
        go' prog state NEG = Just $ nextInstruction prog state { acc = -(acc state) }
        go' prog state SWP = Just $ nextInstruction prog state { bak = acc state, acc = bak state }
        go' prog state SAV = Just $ nextInstruction prog state { bak = acc state }
        go' prog state NOP = Just $ nextInstruction prog state
        go' prog state HCF = Just $ state { mode = ONFIRE }

step :: Cpu Int Int -> (Bool, Cpu Int Int)
step = stepRun . stepRead . stepPrepareRead

stepN :: Cpu Int Int -> Int -> (Cpu Int Int)
stepN cpu n | n <= 0 = cpu
stepN cpu n = case step cpu of
    (True, cpu') -> cpu'
    (False, cpu') -> stepN cpu' (n-1)

steps :: Cpu Int Int -> [Cpu Int Int]
steps cpu = cpu : case step cpu of
    (True, cpu') -> repeat cpu'
    (False, cpu') -> steps cpu'

padLine line = take 22 $ line ++ repeat ' '

nodeToStrings :: Node Int Int -> [String]
nodeToStrings BrokenNode = replicate 15 (replicate 18 '.' ++ "    ")
nodeToStrings (InputNode xs) = (++ [padLine "  \\/"]) $ take 14 $ reverse (map formatItem xs) ++ repeat (replicate 18 '-' ++ "    ")
    where formatItem x = padLine $ show x
nodeToStrings (OutputNode xs) = ([padLine "  \\/"] ++) $ take 14 $ map formatItem xs ++ repeat (replicate 18 '-' ++ "    ")
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
