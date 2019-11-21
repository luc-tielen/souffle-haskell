
{-# Language FlexibleInstances, DerivingVia #-}

module Language.Souffle
  ( Souffle
  , Fact(..)
  , init
  , run
  , loadFiles
  , writeFiles
  , addFact
  , addFacts
  , getFacts
  ) where

import Prelude hiding ( init )
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.RWS
import Data.Foldable ( traverse_ )
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Int
import qualified Language.Souffle.Internal as Internal


-- TODO import Language.Souffle.Monad here, and dont use IO directly

newtype Souffle = Souffle (ForeignPtr Internal.Souffle)

type Tuple = Ptr Internal.Tuple

newtype MarshalT m a = MarshalT (ReaderT Tuple m a)
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadReader Tuple, MonadWriter w
           , MonadState s, MonadRWS Tuple w s, MonadError e )
  via ( ReaderT Tuple m )
  deriving ( MonadTrans ) via (ReaderT Tuple )

runMarshalT :: MarshalT m a -> Tuple -> m a
runMarshalT (MarshalT m) = runReaderT m


class Fact a where
  push :: MonadIO m => a -> MarshalT m ()
  pop :: MonadIO m => MarshalT m a

instance Fact Int32 where
  push int = do
    tuple <- ask
    liftIO $ Internal.tuplePushInt tuple int
  pop = do
    tuple <- ask
    liftIO $ Internal.tuplePopInt tuple

instance Fact String where
  push str = do
    tuple <- ask
    liftIO $ Internal.tuplePushString tuple str
  pop = do
    tuple <- ask
    liftIO $ Internal.tuplePopString tuple


init :: String -> IO (Maybe Souffle)
init prog = fmap Souffle <$> Internal.init prog

run :: Souffle -> IO ()
run (Souffle prog) = Internal.run prog

loadFiles :: Souffle -> String -> IO ()
loadFiles (Souffle prog) = Internal.loadAll prog

writeFiles :: Souffle -> IO ()
writeFiles (Souffle prog) = Internal.printAll prog

getFacts :: Fact a => Souffle -> String -> IO [a]
getFacts (Souffle prog) relationName = do
  relation <- Internal.getRelation prog relationName
  Internal.getRelationIterator relation >>= go []
  where
    go acc it = do
      hasNext <- Internal.relationIteratorHasNext it
      if hasNext
        then do
          tuple <- Internal.relationIteratorNext it
          result <- runMarshalT pop tuple
          go (result : acc) it
        else pure acc

addFact :: Fact a => Souffle -> String -> a -> IO ()
addFact (Souffle prog) relationName fact = do
  relation <- Internal.getRelation prog relationName
  addFact' relation fact

addFacts :: Fact a => Souffle -> String -> [a] -> IO ()
addFacts (Souffle prog) relationName facts = do
  relation <- Internal.getRelation prog relationName
  traverse_ (addFact' relation) facts

addFact' :: Fact a => Ptr Internal.Relation -> a -> IO ()
addFact' relation fact = do
  tuple <- Internal.allocTuple relation
  withForeignPtr tuple $ runMarshalT (push fact)
  Internal.addTuple relation tuple

