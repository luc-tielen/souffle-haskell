
{-# LANGUAGE TemplateHaskell #-}

module Main ( main ) where

import Prelude hiding ( init )
import Language.Datalog.Internal.API
import Foreign.Ptr
import Language.Haskell.TH.Syntax

[] <$ qAddForeignFilePath LangCxx "path.cpp"


-- TODO use ContT for better API

addEdge :: Ptr Souffle -> (String, String) -> IO ()
addEdge prog (from, to) = do
  edge <- getRelation prog "edge"
  tuple <- allocTuple edge
  tuplePushString tuple from
  tuplePushString tuple to
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
  _ <- traverse print results

  free prog

