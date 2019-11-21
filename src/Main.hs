
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Main ( main ) where

import Prelude hiding ( init )
import Data.Foldable ( traverse_ )
import Language.Datalog.TH
import Language.Datalog

embedProgram "path.cpp"


data Edge = Edge String String
  deriving (Eq, Show)

data Reachable = Reachable String String
  deriving (Eq, Show)

instance Fact Edge where
  push (Edge x y) = do
    push x
    push y
  pop = Edge <$> pop <*> pop

instance Fact Reachable where
  push (Reachable x y) = do
    push x
    push y
  pop = Reachable <$> pop <*> pop


main :: IO ()
main = do
  maybeProg <- init "path"
  case maybeProg of
    Nothing -> putStrLn "Failed to load program."
    Just prog -> do
      relation <- getRelation prog "edge"
      addFact relation $ Edge "d" "some_other_node"
      run prog
      reachable <- getRelation prog "reachable"
      results :: [Reachable] <- gatherResults reachable
      traverse_ print results

