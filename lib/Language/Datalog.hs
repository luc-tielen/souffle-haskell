
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
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.Int
import qualified Language.Datalog.Internal.API as API


-- TODO import Language.Datalog.Monad here, and dont use IO directly

newtype Program = Program (ForeignPtr API.Program)
newtype Relation = Relation (Ptr API.Relation)

class Fact a where
  push :: Ptr API.Tuple -> a -> IO ()
  pop :: Ptr API.Tuple -> IO a

instance Fact Int32 where
  push = API.tuplePushInt
  pop = API.tuplePopInt

instance Fact String where
  push = API.tuplePushString
  pop = API.tuplePopString


init :: String -> IO Program
init prog = Program <$> API.init prog

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
          result <- pop tuple
          go (result : acc) it
        else pure acc

addFact :: Fact a => Relation -> a -> IO ()
addFact (Relation relation) fact = do
  tuple <- API.allocTuple relation
  withForeignPtr tuple $ flip push fact
  API.addTuple relation tuple

