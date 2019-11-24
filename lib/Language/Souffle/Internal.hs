
{-# Language LambdaCase #-}

module Language.Souffle.Internal
  ( Souffle
  , Relation
  , RelationIterator
  , Tuple
  , init
  , setNumThreads
  , getNumThreads
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
import Data.Word
import Data.Int
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Language.Souffle.Internal.Bindings as Bindings
import Language.Souffle.Internal.Bindings
  ( Souffle, Relation, RelationIterator, Tuple )


init :: String -> IO (Maybe (ForeignPtr Souffle))
init prog = do
  ptr <- withCString prog Bindings.init
  if ptr == nullPtr
    then pure Nothing
    else Just <$> newForeignPtr Bindings.free ptr
{-# INLINABLE init #-}

setNumThreads :: ForeignPtr Souffle -> Word64 -> IO ()
setNumThreads prog numThreads = withForeignPtr prog $ \ptr ->
    Bindings.setNumThreads ptr $ CSize numThreads
{-# INLINABLE setNumThreads #-}

getNumThreads :: ForeignPtr Souffle -> IO Word64
getNumThreads prog = withForeignPtr prog $ \ptr -> do
    (CSize numThreads) <- Bindings.getNumThreads ptr
    pure numThreads
{-# INLINABLE getNumThreads #-}

run :: ForeignPtr Souffle -> IO ()
run prog = withForeignPtr prog Bindings.run
{-# INLINABLE run #-}

loadAll :: ForeignPtr Souffle -> String -> IO ()
loadAll prog str = withForeignPtr prog $ withCString str . Bindings.loadAll
{-# INLINABLE loadAll #-}

printAll :: ForeignPtr Souffle -> IO ()
printAll prog = withForeignPtr prog Bindings.printAll
{-# INLINABLE printAll #-}

getRelation :: ForeignPtr Souffle -> String -> IO (Ptr Relation)
getRelation prog relation = withForeignPtr prog $ \ptr ->
  withCString relation $ Bindings.getRelation ptr
{-# INLINABLE getRelation #-}

getRelationIterator :: Ptr Relation -> IO (ForeignPtr RelationIterator)
getRelationIterator relation =
  Bindings.getRelationIterator relation >>= newForeignPtr Bindings.freeRelationIterator
{-# INLINABLE getRelationIterator #-}

relationIteratorHasNext :: ForeignPtr RelationIterator -> IO Bool
relationIteratorHasNext iter = withForeignPtr iter $ \ptr ->
  Bindings.relationIteratorHasNext ptr <&> \case
    CBool 0 -> False
    CBool _ -> True
{-# INLINABLE relationIteratorHasNext #-}

relationIteratorNext :: ForeignPtr RelationIterator -> IO (Ptr Tuple)
relationIteratorNext iter = withForeignPtr iter Bindings.relationIteratorNext
{-# INLINABLE relationIteratorNext #-}

allocTuple :: Ptr Relation -> IO (ForeignPtr Tuple)
allocTuple relation =
  Bindings.allocTuple relation >>= newForeignPtr Bindings.freeTuple
{-# INLINABLE allocTuple #-}

addTuple :: Ptr Relation -> ForeignPtr Tuple -> IO ()
addTuple relation tuple =
  withForeignPtr tuple $ Bindings.addTuple relation
{-# INLINABLE addTuple #-}

tuplePushInt :: Ptr Tuple -> Int32 -> IO ()
tuplePushInt tuple i = Bindings.tuplePushInt tuple (CInt i)
{-# INLINABLE tuplePushInt #-}

tuplePushString :: Ptr Tuple -> String -> IO ()
tuplePushString tuple str =
  withCString str $ Bindings.tuplePushString tuple
{-# INLINABLE tuplePushString #-}

tuplePopInt :: Ptr Tuple -> IO Int32
tuplePopInt tuple = alloca $ \ptr -> do
  Bindings.tuplePopInt tuple ptr
  (CInt res) <- peek ptr
  pure res
{-# INLINABLE tuplePopInt #-}

tuplePopString :: Ptr Tuple -> IO String
tuplePopString tuple = alloca $ \ptr -> do
  Bindings.tuplePopString tuple ptr
  cstr <- peek ptr
  str <- peekCString cstr
  free cstr
  pure str
{-# INLINABLE tuplePopString #-}

