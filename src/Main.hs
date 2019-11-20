
{-# LANGUAGE TemplateHaskell #-}

module Main ( main ) where

import Prelude hiding ( init )
import Data.Foldable ( traverse_ )
import Language.Datalog.Internal.API
import Language.Datalog.TH
import Foreign.Ptr
import Foreign.ForeignPtr

embedSouffleProgram "path.cpp"


addEdge :: ForeignPtr Souffle -> (String, String) -> IO ()
addEdge prog (from, to) = do
  edge <- getRelation prog "edge"
  tuple <- allocTuple edge
  withForeignPtr tuple $ \ptr -> do
    tuplePushString ptr from
    tuplePushString ptr to
  addTuple edge tuple

gatherResults :: Ptr Relation -> IO [(String, String)]
gatherResults relation = getRelationIterator relation >>= go []
  where
    go acc it = do
      hasNext <- relationIteratorHasNext it
      if hasNext
        then do
          tuple <- relationIteratorNext it
          str1 <- tuplePopString tuple
          str2 <- tuplePopString tuple
          go ((str1, str2):acc) it
        else pure acc

main :: IO ()
main = do
  prog <- init "path"   -- TODO function that checks if it succeeded
  addEdge prog ("d", "some_other_node")
  run prog
  reachable <- getRelation prog "reachable"
  results <- gatherResults reachable
  traverse_ print results

