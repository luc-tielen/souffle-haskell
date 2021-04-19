
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
  , ByteBuf
  , init
  , setNumThreads
  , getNumThreads
  , run
  , loadAll
  , printAll
  , getRelation
  , pushFacts
  , popFacts
  , containsFact
  ) where

import Prelude hiding ( init )
import Data.Functor ( (<&>) )
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Language.Souffle.Internal.Bindings as Bindings
import Language.Souffle.Internal.Bindings
  ( Souffle, Relation, ByteBuf )
import Control.Exception (mask_)


{- | Initializes a Souffle program.

     The string argument is the name of the program and should be the same
     as the filename (minus the .dl extension).

     The action will return 'Nothing' if it failed to load the Souffle program.
     Otherwise it will return a pointer that can be used in other functions
     in this module.
-}
init :: String -> IO (Maybe (ForeignPtr Souffle))
init prog = mask_ $ do
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

{-| Serializes many facts from Datalog to Haskell.

    You need to check if the passed pointers are non-NULL before passing it
    to this function. Not doing so results in undefined behavior.
    Passing in a different count of objects to what is actually inside the
    byte buffer will crash.
-}
pushFacts :: Ptr Relation -> Ptr ByteBuf -> Word64 -> IO ()
pushFacts relation buf x =
  Bindings.pushByteBuf relation buf (CSize x)
{-# INLINABLE pushFacts #-}

{-| Serializes many facts from Haskell to Datalog.

    You need to check if the passed pointer is non-NULL before passing it
    to this function. Not doing so results in undefined behavior.

    Returns a pointer to a byte buffer that contains the serialized Datalog facts.
-}
popFacts :: Ptr Relation -> IO (ForeignPtr ByteBuf)
popFacts relation = mask_ $ do
  buf <- Bindings.popByteBuf relation
  newForeignPtr Bindings.freeByteBuf buf
{-# INLINABLE popFacts #-}

{- | Checks if a relation contains a certain tuple.

     Returns True if the tuple was found in the relation; otherwise False.
-}
containsFact :: Ptr Relation -> Ptr ByteBuf -> IO Bool
containsFact relation buf =
  Bindings.containsTuple relation buf <&> \case
    CBool 0 -> False
    CBool _ -> True
{-# INLINABLE containsFact #-}

