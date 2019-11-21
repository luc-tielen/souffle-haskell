
{-# Language FlexibleInstances, DerivingVia #-}

module Language.Datalog
  ( Program
  , Relation
  , Fact(..)
  , init
  , run
  , loadAll
  , printAll
  , getRelation
  , gatherResults
  , addFact
  ) where

import Prelude hiding ( init )
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.RWS
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Int
import qualified Language.Datalog.Internal.API as API


-- TODO import Language.Datalog.Monad here, and dont use IO directly

newtype Program = Program (ForeignPtr API.Program)
newtype Relation = Relation (Ptr API.Relation)


newtype MarshalT m a = MarshalT (ReaderT (Ptr API.Tuple) m a)
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadReader (Ptr API.Tuple), MonadWriter w
           , MonadState s, MonadRWS (Ptr API.Tuple) w s, MonadError e)
  via (ReaderT (Ptr API.Tuple) m)
  deriving ( MonadTrans ) via (ReaderT (Ptr API.Tuple))

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

getRelation :: Program -> String -> IO Relation
getRelation (Program prog) name = Relation <$> API.getRelation prog name

gatherResults :: Fact a => Relation -> IO [a]
gatherResults (Relation relation) = API.getRelationIterator relation >>= go []
  where
    go acc it = do
      hasNext <- API.relationIteratorHasNext it
      if hasNext
        then do
          tuple <- API.relationIteratorNext it
          result <- runMarshalT pop tuple
          go (result : acc) it
        else pure acc

addFact :: Fact a => Relation -> a -> IO ()
addFact (Relation relation) fact = do
  tuple <- API.allocTuple relation
  withForeignPtr tuple $ runMarshalT (push fact)
  API.addTuple relation tuple

