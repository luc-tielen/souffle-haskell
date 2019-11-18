
module Main ( main ) where

import Prelude hiding ( init )
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc hiding ( free )
import Foreign.Storable

data Souffle
data Relation
data RelationIterator
data Tuple

-- TODO remove next 2, they are hacks..
foreign import ccall unsafe "souffle_init_Sf_path" initPath :: IO ()
foreign import ccall unsafe "souffle_destroy_Sf_path" destroyPath :: IO ()


-- TODO move to correct module, then can be imported qualified
foreign import ccall unsafe "souffle_init" init
  :: CString -> IO (Ptr Souffle)
foreign import ccall unsafe "souffle_free" free
  :: Ptr Souffle -> IO ()
foreign import ccall unsafe "souffle_run" run
  :: Ptr Souffle -> IO ()
foreign import ccall unsafe "souffle_load_all" loadAll
  :: Ptr Souffle -> CString -> IO ()
foreign import ccall unsafe "souffle_print_all" printAll
  :: Ptr Souffle -> IO ()
foreign import ccall unsafe "souffle_relation" getRelation
  :: Ptr Souffle -> CString -> IO (Ptr Relation)
foreign import ccall unsafe "souffle_relation_iterator" getRelationIterator
  :: Ptr Relation -> IO (Ptr RelationIterator)
foreign import ccall unsafe "souffle_relation_iterator_free" freeRelationIterator
  :: Ptr RelationIterator -> IO ()
foreign import ccall unsafe "souffle_relation_iterator_has_next" relationIteratorHasNext
  :: Ptr RelationIterator -> IO CBool
foreign import ccall unsafe "souffle_relation_iterator_next" relationIteratorNext
  :: Ptr RelationIterator -> IO (Ptr Tuple)
foreign import ccall unsafe "souffle_tuple_alloc" allocTuple
  :: Ptr Relation -> IO (Ptr Tuple)
foreign import ccall unsafe "souffle_tuple_free" freeTuple
  :: Ptr Tuple -> IO ()
foreign import ccall unsafe "souffle_tuple_add" addTuple
  :: Ptr Relation -> Ptr Tuple -> IO ()
foreign import ccall unsafe "souffle_tuple_push_int" tuplePushInt
  :: Ptr Tuple -> CInt -> IO ()
foreign import ccall unsafe "souffle_tuple_push_string" tuplePushString
  :: Ptr Tuple -> CString -> IO ()
foreign import ccall unsafe "souffle_tuple_pop_int" tuplePopInt
  :: Ptr Tuple -> Ptr CInt -> IO ()
foreign import ccall unsafe "souffle_tuple_pop_string" tuplePopString
  :: Ptr Tuple -> Ptr CString -> IO ()


-- TODO better names
-- TODO foreign ptr construct
-- TODO use ContT for better API


addEdge :: Ptr Souffle -> (String, String) -> IO ()
addEdge prog (from, to) = do
  edge <- withCString "edge" $ getRelation prog
  tuple <- allocTuple edge
  withCString from $ tuplePushString tuple
  withCString to $ tuplePushString tuple
  addTuple edge tuple
  freeTuple tuple

gatherResults :: Ptr Relation -> IO [(String, String)]
gatherResults relation = do
  iterator <- getRelationIterator relation
  results <- go [] iterator
  freeRelationIterator iterator
  pure results
  where
    go acc it = do
      (CBool hasNext) <- relationIteratorHasNext it
      if hasNext == 1
        then do
          tuple <- relationIteratorNext it
          alloca $ \ptr1 ->
            alloca $ \ptr2 -> do
              tuplePopString tuple ptr1
              tuplePopString tuple ptr2
              str1 <- peek ptr1 >>= peekCString
              str2 <- peek ptr2 >>= peekCString
              go ((str1, str2):acc) it
        else pure acc

main :: IO ()
main = do
  initPath  -- TODO remove hack
  prog <- withCString "path" init  -- TODO function that checks if it succeeded

  addEdge prog ("a", "some_other_node")

  run prog

  reachable <- withCString "reachable" $ getRelation prog
  results <- gatherResults reachable
  _ <- traverse print results

  free prog
  destroyPath  -- TODO remove hack

