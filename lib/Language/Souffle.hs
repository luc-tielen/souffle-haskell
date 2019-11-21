
{-# Language FlexibleInstances, DerivingVia #-}

module Language.Souffle
  ( Program
  , Fact(..)
  , init
  , run
  , loadAll
  , printAll
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
import qualified Language.Souffle.Internal.API as API


-- TODO import Language.Souffle.Monad here, and dont use IO directly

newtype Program = Program (ForeignPtr API.Program)

newtype MarshalT m a = MarshalT (ReaderT (Ptr API.Tuple) m a)
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadReader (Ptr API.Tuple), MonadWriter w
           , MonadState s, MonadRWS (Ptr API.Tuple) w s, MonadError e )
  via ( ReaderT (Ptr API.Tuple) m )
  deriving ( MonadTrans ) via (ReaderT (Ptr API.Tuple) )

runMarshalT :: MarshalT m a -> Ptr API.Tuple -> m a
runMarshalT (MarshalT m) = runReaderT m


class Fact a where
  push :: MonadIO m => a -> MarshalT m ()
  pop :: MonadIO m => MarshalT m a

instance Fact Int32 where
  push int = do
    tuple <- ask
    liftIO $ API.tuplePushInt tuple int
  pop = do
    tuple <- ask
    liftIO $ API.tuplePopInt tuple

instance Fact String where
  push str = do
    tuple <- ask
    liftIO $ API.tuplePushString tuple str
  pop = do
    tuple <- ask
    liftIO $ API.tuplePopString tuple


init :: String -> IO (Maybe Program)
init prog = fmap Program <$> API.init prog

run :: Program -> IO ()
run (Program prog) = API.run prog

loadAll :: Program -> String -> IO ()
loadAll (Program prog) = API.loadAll prog

printAll :: Program -> IO ()
printAll (Program prog) = API.printAll prog

getFacts :: Fact a => Program -> String -> IO [a]
getFacts (Program prog) relationName = do
  relation <- API.getRelation prog relationName
  API.getRelationIterator relation >>= go []
  where
    go acc it = do
      hasNext <- API.relationIteratorHasNext it
      if hasNext
        then do
          tuple <- API.relationIteratorNext it
          result <- runMarshalT pop tuple
          go (result : acc) it
        else pure acc

addFact :: Fact a => Program -> String -> a -> IO ()
addFact (Program prog) relationName fact = do
  relation <- API.getRelation prog relationName
  addFact' relation fact

addFacts :: Fact a => Program -> String -> [a] -> IO ()
addFacts (Program prog) relationName facts = do
  relation <- API.getRelation prog relationName
  traverse_ (addFact' relation) facts

addFact' :: Fact a => Ptr API.Relation -> a -> IO ()
addFact' relation fact = do
  tuple <- API.allocTuple relation
  withForeignPtr tuple $ runMarshalT (push fact)
  API.addTuple relation tuple

