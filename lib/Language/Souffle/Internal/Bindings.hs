
-- | This module provides C bindings exposed by the files in the cbits directory.
--   This is an internal module, that is prone to have frequent changes,
--   use at your own risk.
module Language.Souffle.Internal.Bindings
  ( Souffle
  , Relation
  , RelationIterator
  , Tuple
  , init
  , free
  , setNumThreads
  , getNumThreads
  , run
  , loadAll
  , printAll
  , getRelation
  , getTupleCount
  , getRelationIterator
  , freeRelationIterator
  , relationIteratorNext
  , allocTuple
  , freeTuple
  , addTuple
  , containsTuple
  , tuplePushInt
  , tuplePushString
  , tuplePopInt
  , tuplePopString
  ) where

import Prelude hiding ( init )
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr


-- | A void type, used for tagging a pointer that points to an embedded
--   Souffle program.
data Souffle

-- | A void type, used for tagging a pointer that points to a relation.
data Relation

-- | A void type, used for tagging a pointer that points to an
--   iterator used for iterating over a relation.
data RelationIterator

-- | A void type, used for tagging a pointer that points to a tuple
--   (term used in the Souffle compiler for a fact).
data Tuple


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

{-| Gets the amount of tuples found in a relation.

    You need to check if both passed pointers are not equal to 'nullPtr' before
    passing it to this function. Not doing so results in undefined behavior (in C++).

    Returns the amount of tuples found in a relation.
-}
foreign import ccall unsafe "souffle_relation_tuple_count" getTupleCount
  :: Ptr Relation -> IO CSize

{-| Create an iterator for iterating over the facts of a relation.

    You need to check if the passed pointer is not equal to 'nullPtr' before
    passing it to this function. Not doing so results in undefined behavior (in C++).

    The returned pointer needs to be freed with 'freeRelationIterator'
    after it is no longer needed.
-}
foreign import ccall unsafe "souffle_relation_iterator" getRelationIterator
  :: Ptr Relation -> IO (Ptr RelationIterator)

{-| Frees a pointer previously allocated with 'getRelationIterator'.

    You need to check if the passed pointer is not equal to 'nullPtr' before
    passing it to this function. Not doing so results in undefined behavior (in C++).
-}
foreign import ccall unsafe "&souffle_relation_iterator_free" freeRelationIterator
  :: FunPtr (Ptr RelationIterator -> IO ())

{-| Advances the relation iterator by 1 position.

    You need to check if the passed pointer is not equal to 'nullPtr' before
    passing it to this function. Not doing so results in undefined behavior (in C++).

    Calling this function when there are no more tuples to be returned
    will result in a crash.

    Returns a pointer to the next tuple. This pointer is not allowed to be freed
    as it is managed by the Souffle program already.
-}
foreign import ccall unsafe "souffle_relation_iterator_next" relationIteratorNext
  :: Ptr RelationIterator -> IO (Ptr Tuple)

{-| Allocates memory for a tuple (fact) to be added to a relation.

    You need to check if the passed pointer is not equal to 'nullPtr' before
    passing it to this function. Not doing so results in undefined behavior (in C++).

    Returns a pointer to a new tuple. Use 'freeTuple' when the tuple
    is no longer required.
-}
foreign import ccall unsafe "souffle_tuple_alloc" allocTuple
  :: Ptr Relation -> IO (Ptr Tuple)

{-| Frees memory of a tuple that was previously allocated (in Haskell).

    You need to check if the passed pointer is not equal to 'nullPtr' before
    passing it to this function. Not doing so results in undefined behavior (in C++).
-}
foreign import ccall unsafe "&souffle_tuple_free" freeTuple
  :: FunPtr (Ptr Tuple -> IO ())

{-| Adds a tuple to a relation.

    You need to check if both passed pointers are not equal to 'nullPtr' before
    passing it to this function. Not doing so results in undefined behavior (in C++).
-}
foreign import ccall unsafe "souffle_tuple_add" addTuple
  :: Ptr Relation -> Ptr Tuple -> IO ()

{- | Checks if a relation contains a certain tuple.

     You need to check if the passed pointers are non-NULL before passing it
     to this function. Not doing so results in undefined behavior.

     Returns True if the tuple was found in the relation; otherwise False.
-}
foreign import ccall unsafe "souffle_contains_tuple" containsTuple
  :: Ptr Relation -> Ptr Tuple -> IO CBool

{-| Pushes an integer value into a tuple.

    You need to check if the passed pointer is not equal to 'nullPtr' before
    passing it to this function. Not doing so results in undefined behavior (in C++).

    Pushing an integer value onto a tuple that expects another type results
    in a crash. Pushing a value into a tuple when it already is "full"
    also results in a crash.
-}
foreign import ccall unsafe "souffle_tuple_push_int" tuplePushInt
  :: Ptr Tuple -> CInt -> IO ()

{-| Pushes a string value into a tuple.

    You need to check if the passed pointer is not equal to 'nullPtr' before
    passing it to this function. Not doing so results in undefined behavior (in C++).

    Pushing a string value onto a tuple that expects another type results
    in a crash. Pushing a value into a tuple when it already is "full"
    also results in a crash.
-}
foreign import ccall unsafe "souffle_tuple_push_string" tuplePushString
  :: Ptr Tuple -> CString -> IO ()

{-| Extracts an integer value from a tuple.

    You need to check if the passed pointer is not equal to 'nullPtr' before passing it
    to this function. Not doing so results in undefined behavior.

    Extracting an integer value from a tuple that expects another type results
    in a crash. Extracting a value from a tuple when it is already "empty"
    also results in a crash.

    The popped integer will be stored in the pointer that is passed in.
-}
foreign import ccall unsafe "souffle_tuple_pop_int" tuplePopInt
  :: Ptr Tuple -> Ptr CInt -> IO ()

{-| Extracts a string value from a tuple.

    You need to check if the passed pointer is not equal to 'nullPtr' before passing it
    to this function. Not doing so results in undefined behavior.

    Extracting a string value from a tuple that expects another type results
    in a crash. Extracting a value from a tuple when it is already "empty"
    also results in a crash.

    The popped string will be stored in the result pointer.
-}
foreign import ccall unsafe "souffle_tuple_pop_string" tuplePopString
  :: Ptr Tuple -> Ptr CString -> IO ()

