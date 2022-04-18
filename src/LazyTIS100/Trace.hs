{-# LANGUAGE TupleSections, LambdaCase #-}
{-# LANGUAGE Unsafe #-}
module LazyTIS100.Trace
    ( trace,
      traceS,
      traceId,
      traceIdS,
      traceShow,
      traceShowId,
      traceStack,
      traceIO,

      clearTrace,
      getTrace,
      startTrace,
      restartTrace,
    ) where

import Control.Concurrent.MVar
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import GHC.Stack (currentCallStack, renderStack)
import System.IO.Unsafe (unsafePerformIO)

import LazyTIS100.Prelude

traceDataStore :: MVar (Maybe (Seq.Seq T.Text))
traceDataStore = unsafePerformIO $ newMVar Nothing

traceIO :: T.Text -> IO ()
traceIO msg = modifyMVar_ traceDataStore $ \case
  Nothing -> pure Nothing
  Just seq -> pure $ Just $ seq Seq.|> msg

clearTrace :: IO ()
clearTrace = modifyMVar_ traceDataStore $ const $ pure $ Just $ Seq.empty

startTrace :: IO ()
startTrace = modifyMVar_ traceDataStore $ pure . Just . fromMaybe Seq.empty

getTrace :: IO [T.Text]
getTrace = toList . fromMaybe Seq.empty <$> readMVar traceDataStore

restartTrace :: IO [T.Text]
restartTrace = modifyMVar traceDataStore $ \x -> pure (Just Seq.empty, toList $ fromMaybe Seq.empty x)

-- http://localhost:8080/file/home/parallels/.stack/programs/aarch64-linux/ghc-9.0.2/share/doc/ghc-9.0.2/html/libraries/base-4.15.1.0/src/Debug-Trace.html#traceId
-- (without documentation and with some changes, e.g. use Text in some places)
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}

{-# NOINLINE trace #-}
trace :: T.Text -> a -> a
trace string expr = unsafePerformIO $ do
    traceIO string
    return expr

traceS :: String -> a -> a
traceS string = trace (T.pack string)

traceIdS :: String -> String
traceIdS a = traceS a a

traceId :: T.Text -> T.Text
traceId a = trace a a

traceShow :: Show a => a -> b -> b
traceShow = traceS . show

traceShowId :: Show a => a -> a
traceShowId a = trace (T.pack $ show a) a

traceStack :: T.Text -> a -> a
traceStack str expr = unsafePerformIO $ do
   traceIO str
   stack <- currentCallStack
   when (not (null stack)) $ traceIO (T.pack $ renderStack stack)
   return expr


