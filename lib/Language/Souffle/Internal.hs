
-- | An internal module, providing a slightly higher level interface than
--   "Language.Souffle.Internal.Bindings".
--   It uses more commonly found data types instead of the low level C types
--   for easier integration with other parts of a Haskell application.
--   Also it takes care of garbage collection so other modules do not have
--   to take this into account anymore.
--
--   Used only internally, so prone to changes, use at your own risk.
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
  , countFacts
  , getRelationIterator
  , relationIteratorNext
  , allocTuple
  , addTuple
  , containsTuple
  , tuplePushInt32
  , tuplePushUInt32
  , tuplePushString
  , tuplePopInt32
  , tuplePopUInt32
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


{- | Initializes a Souffle program.

     The string argument is the name of the program and should be the same
     as the filename (minus the .dl extension).

     The action will return 'Nothing' if it failed to load the Souffle program.
     Otherwise it will return a pointer that can be used in other functions
     in this module.
-}
init :: String -> IO (Maybe (ForeignPtr Souffle))
init prog = do
  ptr <- withCString prog Bindings.init
  if ptr == nullPtr
    then pure Nothing
    else Just <$> newForeignPtr Bindings.free ptr
{-# INLINABLE init #-}

-- | Sets the number of CPU cores this Souffle program should use.
setNumThreads :: ForeignPtr Souffle -> Word64 -> IO ()
setNumThreads prog numThreads = withForeignPtr prog $ \ptr ->
    Bindings.setNumThreads ptr $ CSize numThreads
{-# INLINABLE setNumThreads #-}

-- | Gets the number of CPU cores this Souffle program should use.
getNumThreads :: ForeignPtr Souffle -> IO Word64
getNumThreads prog = withForeignPtr prog $ \ptr -> do
    (CSize numThreads) <- Bindings.getNumThreads ptr
    pure numThreads
{-# INLINABLE getNumThreads #-}

-- | Runs the Souffle program.
run :: ForeignPtr Souffle -> IO ()
run prog = withForeignPtr prog Bindings.run
{-# INLINABLE run #-}

-- | Load all facts from files in a certain directory.
loadAll :: ForeignPtr Souffle -> FilePath -> IO ()
loadAll prog inputDir = withForeignPtr prog $ withCString inputDir . Bindings.loadAll
{-# INLINABLE loadAll #-}

-- | Write out all facts of the program to CSV files in a certain directory
--   (as defined in the Souffle program).
printAll :: ForeignPtr Souffle -> FilePath -> IO ()
printAll prog outputDir = withForeignPtr prog $ withCString outputDir . Bindings.printAll
{-# INLINABLE printAll #-}

{-| Lookup a relation by name in the Souffle program.

    Note that the returned pointer can be 'nullPtr' if it is not defined
    in the Souffle program.
-}
getRelation :: ForeignPtr Souffle -> String -> IO (Ptr Relation)
getRelation prog relation = withForeignPtr prog $ \ptr ->
  withCString relation $ Bindings.getRelation ptr
{-# INLINABLE getRelation #-}

-- | Returns the amount of facts found in a relation.
countFacts :: Ptr Relation -> IO Int
countFacts relation =
  Bindings.getTupleCount relation >>= \(CSize count) ->
    -- TODO: check what happens for really large sizes?
    pure (fromIntegral count)

-- | Create an iterator for iterating over the facts of a relation.
getRelationIterator :: Ptr Relation -> IO (ForeignPtr RelationIterator)
getRelationIterator relation =
  Bindings.getRelationIterator relation >>= newForeignPtr Bindings.freeRelationIterator
{-# INLINABLE getRelationIterator #-}

{-| Advances the relation iterator by 1 position.

    Calling this function when there are no more results to be returned
    will result in a crash.
-}
relationIteratorNext :: ForeignPtr RelationIterator -> IO (Ptr Tuple)
relationIteratorNext iter = withForeignPtr iter Bindings.relationIteratorNext
{-# INLINABLE relationIteratorNext #-}

-- | Allocates memory for a tuple (fact) to be added to a relation.
allocTuple :: Ptr Relation -> IO (ForeignPtr Tuple)
allocTuple relation =
  Bindings.allocTuple relation >>= newForeignPtr Bindings.freeTuple
{-# INLINABLE allocTuple #-}

-- | Adds a tuple (fact) to a relation.
addTuple :: Ptr Relation -> ForeignPtr Tuple -> IO ()
addTuple relation tuple =
  withForeignPtr tuple $ Bindings.addTuple relation
{-# INLINABLE addTuple #-}

{- | Checks if a relation contains a certain tuple.

     Returns True if the tuple was found in the relation; otherwise False.
-}
containsTuple :: Ptr Relation -> ForeignPtr Tuple -> IO Bool
containsTuple relation tuple = withForeignPtr tuple $ \ptr ->
  Bindings.containsTuple relation ptr <&> \case
    CBool 0 -> False
    CBool _ -> True
{-# INLINABLE containsTuple #-}

-- | Pushes an integer value into a tuple.
tuplePushInt32 :: Ptr Tuple -> Int32 -> IO ()
tuplePushInt32 tuple i = Bindings.tuplePushInt32 tuple (CInt i)
{-# INLINABLE tuplePushInt32 #-}

-- | Pushes an integer value into a tuple.
tuplePushUInt32 :: Ptr Tuple -> Word32 -> IO ()
tuplePushUInt32 tuple i = Bindings.tuplePushUInt32 tuple (CUInt i)
{-# INLINABLE tuplePushUInt32 #-}

-- | Pushes a string value into a tuple.
tuplePushString :: Ptr Tuple -> String -> IO ()
tuplePushString tuple str =
  withCString str $ Bindings.tuplePushString tuple
{-# INLINABLE tuplePushString #-}

-- | Extracts a 32 bit signed integer value from a tuple.
tuplePopInt32 :: Ptr Tuple -> IO Int32
tuplePopInt32 tuple = alloca $ \ptr -> do
  Bindings.tuplePopInt32 tuple ptr
  (CInt res) <- peek ptr
  pure res
{-# INLINABLE tuplePopInt32 #-}

-- | Extracts a 32 bit unsigned integer value from a tuple.
tuplePopUInt32 :: Ptr Tuple -> IO Word32
tuplePopUInt32 tuple = alloca $ \ptr -> do
  Bindings.tuplePopUInt32 tuple ptr
  (CUInt res) <- peek ptr
  pure res
{-# INLINABLE tuplePopUInt32 #-}

-- | Extracts a string value from a tuple.
tuplePopString :: Ptr Tuple -> IO String
tuplePopString tuple = alloca $ \ptr -> do
  Bindings.tuplePopString tuple ptr
  cstr <- peek ptr
  str <- peekCString cstr
  free cstr
  pure str
{-# INLINABLE tuplePopString #-}

