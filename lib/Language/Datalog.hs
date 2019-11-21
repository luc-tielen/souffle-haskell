
{-# Language FlexibleInstances #-}

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
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Int
import qualified Language.Datalog.Internal.API as API


-- TODO import Language.Datalog.Monad here, and dont use IO directly

newtype Program = Program (ForeignPtr API.Program)
newtype Relation = Relation (Ptr API.Relation)


type MarshalM = ReaderT (Ptr API.Tuple)

class Fact a where
  push :: MonadIO m => a -> MarshalM m ()
  pop :: MonadIO m => MarshalM m a

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
          result <- runReaderT pop tuple
          go (result : acc) it
        else pure acc

addFact :: Fact a => Relation -> a -> IO ()
addFact (Relation relation) fact = do
  tuple <- API.allocTuple relation
  withForeignPtr tuple $ runReaderT (push fact)
  API.addTuple relation tuple

