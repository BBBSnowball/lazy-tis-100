module LazyTIS100.Prelude (
    module E,
    showT, at,
) where

import Control.Exception as E (finally)

import Control.Monad as E (forM, forM_, mapM, mapM_, void, when)
import Control.Monad.Trans.Class as E (MonadTrans, lift)
import Control.Monad.Except as E (MonadError, ExceptT, runExceptT, throwError, catchError)
import Control.Monad.Reader as E (MonadReader, ReaderT, runReaderT)
import Control.Monad.State as E (MonadState, State, StateT, runStateT, runState)
import Control.Monad.Writer as E (Writer, execWriter, tell)
import qualified Control.Monad.Reader
import qualified Control.Monad.State

import qualified Data.Array as A
import Data.Array as E (Array, (//))
import Data.Either as E (either)
import Data.Foldable as E (toList)
import Data.Maybe as E (catMaybes, fromMaybe)
import Data.List as E (transpose)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Text as E (Text, pack, unpack)

import Generic.Data as E (Generic, Generic1, gfmap)
import Generic.Functor as E (gbimap, gbifoldMap, gbitraverse)

import Data.Bifunctor as E (Bifunctor (..))
import Data.Bifoldable as E (Bifoldable (..))
import Data.Bitraversable as E (Bitraversable (..))


showT :: Show a => a -> T.Text
showT = T.pack . show

at :: A.Ix i => A.Array i e -> i -> Maybe e
arr `at` ix = if A.inRange (A.bounds arr) ix then Just (arr A.! ix) else Nothing
