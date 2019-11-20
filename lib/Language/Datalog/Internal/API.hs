
{-# Language LambdaCase #-}

-- TODO module name?
module Language.Datalog.Internal.API
  ( Souffle
  , Relation
  , RelationIterator
  , Tuple
  , init
  , run
  , loadAll
  , printAll
  , getRelation
  , getRelationIterator
  , relationIteratorHasNext
  , relationIteratorNext
  , allocTuple
  , addTuple
  , tuplePushInt
  , tuplePushString
  , tuplePopInt
  , tuplePopString
  ) where

import Prelude hiding ( init )
import Data.Functor ( (<&>) )
import Data.Int
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Language.Datalog.Internal.Bindings as Bindings
import Language.Datalog.Internal.Bindings
  ( Souffle, Relation, RelationIterator, Tuple )


init :: String -> IO (ForeignPtr Souffle)
init prog =
  withCString prog Bindings.init >>= newForeignPtr Bindings.free

run :: ForeignPtr Souffle -> IO ()
run prog = withForeignPtr prog Bindings.run

loadAll :: ForeignPtr Souffle -> String -> IO ()
loadAll prog str = withForeignPtr prog $ withCString str . Bindings.loadAll

printAll :: ForeignPtr Souffle -> IO ()
printAll prog = withForeignPtr prog Bindings.printAll

getRelation :: ForeignPtr Souffle -> String -> IO (Ptr Relation)
getRelation prog relation = withForeignPtr prog $ \ptr ->
  withCString relation $ Bindings.getRelation ptr

getRelationIterator :: Ptr Relation -> IO (ForeignPtr RelationIterator)
getRelationIterator relation =
  Bindings.getRelationIterator relation >>= newForeignPtr Bindings.freeRelationIterator

relationIteratorHasNext :: ForeignPtr RelationIterator -> IO Bool
relationIteratorHasNext iter = withForeignPtr iter $ \ptr ->
  Bindings.relationIteratorHasNext ptr <&> \case
    CBool 0 -> False
    CBool _ -> True

relationIteratorNext :: ForeignPtr RelationIterator -> IO (Ptr Tuple)
relationIteratorNext iter = withForeignPtr iter Bindings.relationIteratorNext

allocTuple :: Ptr Relation -> IO (ForeignPtr Tuple)
allocTuple relation =
  Bindings.allocTuple relation >>= newForeignPtr Bindings.freeTuple

addTuple :: Ptr Relation -> ForeignPtr Tuple -> IO ()
addTuple relation tuple =
  withForeignPtr tuple $ Bindings.addTuple relation

tuplePushInt :: Ptr Tuple -> Int32 -> IO ()
tuplePushInt tuple i = Bindings.tuplePushInt tuple (CInt i)

tuplePushString :: Ptr Tuple -> String -> IO ()
tuplePushString tuple str =
  withCString str $ Bindings.tuplePushString tuple

tuplePopInt :: Ptr Tuple -> IO Int32
tuplePopInt tuple = alloca $ \ptr -> do
    Bindings.tuplePopInt tuple ptr
    (CInt res) <- peek ptr
    pure res

tuplePopString :: Ptr Tuple -> IO String
tuplePopString tuple = alloca $ \ptr -> do
    Bindings.tuplePopString tuple ptr
    cstr <- peek ptr
    str <- peekCString cstr
    free cstr
    pure str

