
-- | This module provides C bindings exposed by the files in the cbits directory.
--   This is an internal module, that is prone to have frequent changes,
--   use at your own risk.
module Language.Souffle.Internal.Bindings
  ( Souffle
  , Relation
  , ByteBuf
  , init
  , free
  , setNumThreads
  , getNumThreads
  , run
  , loadAll
  , printAll
  , getRelation
  , pushByteBuf
  , popByteBuf
  , containsTuple
  ) where

import Prelude hiding ( init )
import Data.Kind (Type)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr


-- | A void type, used for tagging a pointer that points to an embedded
--   Souffle program.
type Souffle :: Type
data Souffle

-- | A void type, used for tagging a pointer that points to a relation.
type Relation :: Type
data Relation

-- | A void type, used for tagging a pointer that points to a raw bytearray.
type ByteBuf :: Type
data ByteBuf


{- | Initializes a Souffle program.

     The string argument is the name of the program and should be the same
     as the filename (minus the .dl extension).
     The pointer that is returned can be 'nullPtr' in case something went wrong.
     If a valid pointer is returned, it needs to be freed by 'free'
     after it is no longer needed.
-}
foreign import ccall unsafe "souffle_init" init
  :: CString -> IO (Ptr Souffle)

{-| Frees the memory in use by the pointer, previously allocated by 'init'.

    You need to check if the pointer is not equal to 'nullPtr'
    before passing it to this function. Not doing so results in
    undefined behavior (in C++).
-}
foreign import ccall unsafe "&souffle_free" free
  :: FunPtr (Ptr Souffle -> IO ())

{-| Sets the number of CPU cores this Souffle program should use.

    You need to check if the pointer is not equal to 'nullPtr'
    before passing it to this function. Not doing so results in
    undefined behavior (in C++).
-}
foreign import ccall unsafe "souffle_set_num_threads" setNumThreads
  :: Ptr Souffle -> CSize -> IO ()

{-| Gets the number of CPU cores this Souffle program should use.

    You need to check if the pointer is equal to 'nullPtr' before passing
    it to this function. Not doing so results in undefined behavior (in C++).
-}
foreign import ccall unsafe "souffle_get_num_threads" getNumThreads
  :: Ptr Souffle -> IO CSize

{-| Runs the Souffle program.

    You need to check if the pointer is equal to 'nullPtr' before passing
    it to this function. Not doing so results in undefined behavior (in C++).
-}
foreign import ccall unsafe "souffle_run" run
  :: Ptr Souffle -> IO ()

{-| Load all facts from files in a certain directory.

    You need to check if both pointers are not equal to 'nullPtr' before passing
    it to this function. Not doing so results in undefined behavior (in C++).
-}
foreign import ccall unsafe "souffle_load_all" loadAll
  :: Ptr Souffle -> CString -> IO ()

{-| Write out all facts of the program to CSV files in a given directory
    (as defined in the Souffle program).

    You need to check if the pointer is not equal to 'nullPtr' before passing it
    to this function. Not doing so results in undefined behavior (in C++).
-}
foreign import ccall unsafe "souffle_print_all" printAll
  :: Ptr Souffle -> CString -> IO ()

{-| Lookup a relation in the Souffle program.

    You need to check if both passed pointers are not equal to 'nullPtr' before
    passing it to this function. Not doing so results in undefined behavior (in C++).

    The returned pointer can be 'nullPtr' if the relation is not found.
    The pointer does not need to be freed, it is managed by the Souffle program.
-}
foreign import ccall unsafe "souffle_relation" getRelation
  :: Ptr Souffle -> CString -> IO (Ptr Relation)

{-| Checks if a relation contains a certain tuple.

    You need to check if the passed pointers are non-NULL before passing it
    to this function. Not doing so results in undefined behavior.

    Returns True if the tuple was found in the relation; otherwise False.
-}
foreign import ccall unsafe "souffle_contains_tuple" containsTuple
  :: Ptr Relation -> Ptr ByteBuf -> IO CBool

{-| Serializes many Datalog facts from Haskell to C++.

    You need to check if the passed pointers are non-NULL before passing it
    to this function. Not doing so results in undefined behavior.
    Passing in a different count of objects to what is actually inside the
    byte buffer will crash.
-}
foreign import ccall unsafe "souffle_tuple_push_many" pushByteBuf
  :: Ptr Relation -> Ptr ByteBuf -> CSize -> IO ()

{-| Serializes many Datalog facts from Datalog to Haskell

    You need to check if the passed pointers are non-NULL before passing it
    to this function. Not doing so results in undefined behavior.

    Returns a pointer to a byte buffer that contains the serialized Datalog facts.
-}
foreign import ccall unsafe "souffle_tuple_pop_many" popByteBuf
  :: Ptr Souffle -> Ptr Relation -> IO (Ptr ByteBuf)

