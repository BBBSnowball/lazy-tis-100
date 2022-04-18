{-# LANGUAGE TupleSections, LambdaCase, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts,
    ScopedTypeVariables, NamedFieldPuns, OverloadedStrings #-}
module LazyTIS100.EvalEager
    ( portOrderReadAny,
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
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import LazyTIS100.Types
import LazyTIS100.Trace


-- https://kk4ead.github.io/tis-100/
portOrderReadAny = [ LEFT, RIGHT, UP, DOWN ]

data NoEvalReason = SkipNode

--FIXME make transformers so actions can stack this over Writer or IO
newtype TISEval l n a = TISEval { runTISEval :: ExceptT NoEvalReason (State (Cpu l n)) a }
    deriving (Functor, Applicative, Monad, MonadError NoEvalReason, MonadState (Cpu l n))

newtype TISEvalForNode l n a = TISEvalForNode { runTISEvalForNode :: ReaderT NodeIndex (StateT (Bool, Node l n) (ExceptT NoEvalReason (State (Cpu l n)))) a }
    deriving (Functor, Applicative, Monad, MonadError NoEvalReason, MonadReader NodeIndex)

instance MonadFail (TISEvalForNode l n) where
    fail _ = throwError SkipNode

instance MonadState (Cpu l n) (TISEvalForNode l n) where
    state = TISEvalForNode . lift . lift . Control.Monad.State.state

class Monad m => MonadTISEval l n m | m -> l n where
    getCpu :: m (Cpu l n)
    --FIXME buffer updates until call to `commit`, which tells us that previous updates must be visible to other nodes
    maybeUpdateNode :: NodeIndex -> (Node l n -> Maybe (Node l n, a)) -> m (Maybe a)

maybeUpdateNodeImpl :: MonadState (Cpu l n) m => NodeIndex -> (Node l n -> Maybe (Node l n, a)) -> m (Maybe a)
maybeUpdateNodeImpl ix f = do
    cpu <- Control.Monad.State.get
    case (cpu `at` ix) >>= f of
        Nothing -> pure Nothing
        Just (node', result) -> do
            ($!) Control.Monad.State.put (cpu // [(ix, node')])
            pure $ Just result

instance MonadTISEval l n (TISEval l n) where
    getCpu = Control.Monad.State.get
    maybeUpdateNode = maybeUpdateNodeImpl

instance MonadTISEval l n (TISEvalForNode l n) where
    getCpu = Control.Monad.State.get
    maybeUpdateNode = maybeUpdateNodeImpl

maybeUpdateNode_ :: MonadTISEval l n m => NodeIndex -> (Node l n -> Maybe (Node l n)) -> m ()
maybeUpdateNode_ ix f = void $ maybeUpdateNode ix (fmap (,()) . f)

updateNode :: MonadTISEval l n m => NodeIndex -> Node l n -> m ()
updateNode ix node' = maybeUpdateNode ix (fmap (,()) . Just . const node') >>= \case
    Just () -> pure ()
    Nothing -> error "invalid node index in updateNode"

class MonadTISEval l n m => MonadTISEvalForNode l n m | m -> l n where
    getCurrentNodeIndex :: m NodeIndex
    getCurrentNode :: m (Node l n)
    updateCurrentNode :: Node l n -> m ()

instance MonadTISEvalForNode l n (TISEvalForNode l n) where
    getCurrentNodeIndex = Control.Monad.Reader.ask
    getCurrentNode = fmap snd $ TISEvalForNode $ Control.Monad.State.get
    updateCurrentNode node' = TISEvalForNode $ Control.Monad.State.put (True, node')

catchSkip :: TISEvalForNode l n () -> TISEvalForNode l n ()
catchSkip = flip catchError $ \case
    SkipNode -> pure ()
    --e -> throwError e  -- redundant until we have more error types

traceM :: MonadState s m => T.Text -> m ()
--traceM msg = Control.Monad.State.get >>= \x -> Control.Monad.State.put (trace msg x)
traceM msg = Control.Monad.State.modify (trace msg)

showT :: Show a => a -> T.Text
showT = T.pack . show

foreachNode :: Integral n => TISEvalForNode l n () -> TISEval l n ()
foreachNode action = getCpu >>= \cpu -> forM_ (A.assocs cpu) go
    where
        go (ix, node) =
            let dotrace = traceM $ "foreachNode: " <> showT ix
                step1 = runReaderT (runTISEvalForNode $ catchSkip $ dotrace >> action) ix
                step2 = runStateT step1 (False, node)
            in TISEval step2 >>= \((), (updated, node')) ->
                when updated $ do
                    case node' of
                        ComputeNode _ NodeState {mode} -> traceM $ "  updated: mode=" <> showT (fmap toInteger mode)
                        _ -> traceM "  updated"
                    updateNode ix node'

runTISEvalForCpu :: TISEval l n a -> Cpu l n -> (Either NoEvalReason a, Cpu l n)
runTISEvalForCpu action cpu = runState (runExceptT (runTISEval action)) cpu

runTISEvalForCpu_ :: TISEval l n a -> Cpu l n -> Cpu l n
runTISEvalForCpu_ action cpu = snd $ runState (runExceptT (runTISEval action)) cpu


stepPrepareReadForNode :: Show n => TISEvalForNode l n ()
stepPrepareReadForNode = do
    --NOTE Failed pattern matches will skip processing for this node.
    ComputeNode prog state@(NodeState {mode}) <- getCurrentNode
    case mode of
        WRITE _ _ -> throwError SkipNode  -- don't short-circuit write
        READ _ -> throwError SkipNode  -- change would be redundant
        _ -> pure ()
    Just inst <- pure $ prog `at` pc state
    Just (SPort port) <- pure $ instructionSource inst
    traceM $ "  start reading, mode was " <> showT mode
    updateCurrentNode $ ComputeNode prog state { mode = READ port }

neighbourIndex :: NodeIndex -> Port -> (NodeIndex, Port, Port)
neighbourIndex (y, x) LEFT  = ((y, x-1), LEFT,  RIGHT)
neighbourIndex (y, x) RIGHT = ((y, x+1), RIGHT, LEFT)
neighbourIndex (y, x) UP    = ((y-1, x), UP,    DOWN)
neighbourIndex (y, x) DOWN  = ((y+1, x), DOWN,  UP)
neighbourIndex (y, x) z     = ((y, x), z, z)  -- usually invalid but allowed special case for LAST

isValidNodeIndex :: Cpu l n -> NodeIndex -> Bool
isValidNodeIndex cpu ix = A.inRange (A.bounds cpu) ix

neighbourIndicesForRead :: NodeIndex -> Port -> Port -> [(NodeIndex, Port, Port)]
neighbourIndicesForRead myIndex lastPort port = map (neighbourIndex myIndex) $ applicableNeighbours port
    where
        applicableNeighbours ANY = portOrderReadAny
        applicableNeighbours LAST = [lastPort]
        applicableNeighbours p = [p]

at :: A.Ix i => Array i e -> i -> Maybe e
arr `at` ix = if A.inRange (A.bounds arr) ix then Just (arr A.! ix) else Nothing

tryRead :: (Num n, Show n) => Port -> Port -> TISEvalForNode l n (Maybe (Port, n))
tryRead lastPort port = do
    ix <- getCurrentNodeIndex
    result <- firstJustsM . map tryReadOne $ neighbourIndicesForRead ix lastPort port
    traceM $ "  tryRead " <> showT ix <> ": " <> showT result
    pure result
    where
        firstJustsM :: Monad m => [m (Maybe a)] -> m (Maybe a)
        firstJustsM [] = pure Nothing
        firstJustsM (x : xs) = x >>= \case
            Just y -> pure $ Just y
            Nothing -> firstJustsM xs

        -- special case: LAST used without preceding ANY is like NIL
        tryReadOne (theirIndex, LAST, LAST) = pure $ Just (LAST, fromInteger 0)
        tryReadOne (theirIndex, myPort, theirPort) = maybeUpdateNode theirIndex $ \case
            --FIXME I think input nodes also can only write every second cycle. We have to consider this.
            InputNode (v : vs) ->
                trace ("    tryReadOne " <> showT theirIndex <> ", input") $
                Just (InputNode vs, (myPort, v))
            ComputeNode prog theirState@(NodeState {mode = WRITE ANY   v}) ->
                trace ("    tryReadOne " <> showT theirIndex <> ", WRITE ANY") $
                let node' = ComputeNode prog (theirState {mode = HasWritten, lastPort = theirPort})
                in Just (node', (myPort, v))
            ComputeNode prog theirState@(NodeState {mode = WRITE port' v}) | port' == theirPort ->
                trace ("    tryReadOne " <> showT theirIndex <> ", WRITE " <> showT port') $
                let node' = ComputeNode prog (theirState {mode = HasWritten})
                in Just (node', (myPort, v))
            ComputeNode prog theirState@(NodeState {mode}) ->
                trace ("    tryReadOne " <> showT theirIndex <> ", nope, " <> showT mode) $
                Nothing
            _ -> Nothing

stepReadForNode :: (Num n, Show n) => TISEvalForNode l n ()
stepReadForNode = getCurrentNode >>= \case
        ComputeNode prog state@(NodeState {mode = READ port}) -> do
            Just (actualPort, value) <- tryRead (lastPort state) port
            traceM "  read successful"
            updateCurrentNode (ComputeNode prog state {mode = HasRead value, lastPort = if port == ANY then actualPort else lastPort state})
        node@(OutputNode {outputNodeCapacity, outputNodeActual})
            | outputNodeCapacity > 0 -> do
                Just (actualPort, value) <- tryRead UP UP
                traceM "  read successful for output node"
                updateCurrentNode $ node
                    { outputNodeCapacity = outputNodeCapacity - 1
                    , outputNodeActual = outputNodeActual Seq.|> value }
        _ -> pure ()

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

        go :: (NodeIndex, Node Int Int) -> Maybe (NodeIndex, Node Int Int)
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

        isSourceConstant state (SPort LAST) = lastPort state == LAST
        isSourceConstant state (SPort _) = False
        isSourceConstant state (SImmediate _) = True
        isSourceConstant state SAcc = True
        isSourceConstant state SNil = True

        go' :: NodeProgram Int Int -> NodeState Int -> Instruction Int Int -> Maybe (NodeState Int)
        go' prog NodeState { mode = FINISHED } _ = Nothing
        go' prog state@NodeState { mode = HasWritten } _ = Just $ nextInstruction prog state
        go' prog state (JMP lbl) =
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
            Just $ state { pc = newPc, mode = if newPc == pc state && isSourceConstant state src then FINISHED else RUN }
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
--NOTE Read and Run must be in separate calls to foreachNode (i.e. first process all reads, then start run and write)
--     so other nodes only see writes in the next step.
step = stepRun . runTISEvalForCpu_ (foreachNode $ catchSkip stepPrepareReadForNode >> catchSkip stepReadForNode)

stepN :: Int -> Cpu Int Int -> (Int, Cpu Int Int)
stepN n cpu | n <= 0 = (-1, cpu)
stepN n cpu = case step cpu of
    (True, cpu') -> (n-1, cpu')
    (False, cpu') -> stepN (n-1) cpu'

steps :: Cpu Int Int -> [Cpu Int Int]
steps cpu = cpu : case step cpu of
    (True, cpu') -> repeat cpu'
    (False, cpu') -> steps cpu'
