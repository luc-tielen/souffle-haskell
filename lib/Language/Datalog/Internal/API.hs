
{-# Language LambdaCase #-}

-- TODO module name?
module Language.Datalog.Internal.API
  ( init
  , Bindings.free
  , Bindings.run
  , loadAll
  , Bindings.printAll
  , getRelation
  , Bindings.getRelationIterator
  , Bindings.freeRelationIterator
  , relationIteratorHasNext
  , Bindings.relationIteratorNext
  , Bindings.allocTuple
  , Bindings.freeTuple
  , Bindings.addTuple
  , tuplePushInt
  , tuplePushString
  , tuplePopInt
  , tuplePopString
  ) where

import Prelude hiding ( init )
import Data.Functor ((<&>))
import Data.Int
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import qualified Language.Datalog.Internal.Bindings as Bindings
import Language.Datalog.Internal.Bindings
  ( Souffle, Relation, RelationIterator, Tuple )

-- TODO foreign ptrs, also reexports
-- TODO with* construct for program, tuples, ...

init :: String -> IO (Ptr Souffle)
init prog = withCString prog Bindings.init

loadAll :: Ptr Souffle -> String -> IO ()
loadAll prog str = withCString str $ Bindings.loadAll prog

getRelation :: Ptr Souffle -> String -> IO (Ptr Relation)
getRelation prog relation =
  withCString relation $ Bindings.getRelation prog

relationIteratorHasNext :: Ptr RelationIterator -> IO Bool
relationIteratorHasNext iter =
  Bindings.relationIteratorHasNext iter <&> \case
    CBool 0 -> False
    CBool _ -> True

tuplePushInt :: Ptr Tuple -> Int32 -> IO ()
tuplePushInt t i = Bindings.tuplePushInt t (CInt i)

tuplePushString :: Ptr Tuple -> String -> IO ()
tuplePushString t str =
  withCString str $ Bindings.tuplePushString t

tuplePopInt :: Ptr Tuple -> IO Int32
tuplePopInt t = alloca $ \ptr -> do
  Bindings.tuplePopInt t ptr
  (CInt res) <- peek ptr
  pure res

tuplePopString :: Ptr Tuple -> IO String
tuplePopString t = alloca $ \ptr -> do
  Bindings.tuplePopString t ptr
  cstr <- peek ptr
  str <- peekCString cstr
  free cstr
  pure str

