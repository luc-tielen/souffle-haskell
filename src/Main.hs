
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, DataKinds, TypeFamilies #-}

module Main ( main ) where

import Prelude hiding ( init )
import Data.Foldable ( traverse_ )
import Language.Souffle.TH
import Language.Souffle

embedProgram "path.cpp"


data Path = Path

instance Program Path where
  type ProgramName Path = "path"
  type ProgramFacts Path = [Edge, Reachable]

data Edge = Edge String String
  deriving (Eq, Show)

data Reachable = Reachable String String
  deriving (Eq, Show)

instance Marshal Edge where
  push (Edge x y) = do
    push x
    push y
  pop = Edge <$> pop <*> pop

instance Fact Edge where
  type FactName Edge = "edge"

instance Marshal Reachable where
  push (Reachable x y) = do
    push x
    push y
  pop = Reachable <$> pop <*> pop

instance Fact Reachable where
  type FactName Reachable = "reachable"


main :: IO ()
main = do
  maybeProgram <- init Path
  case maybeProgram of
    Nothing -> putStrLn "Failed to load program."
    Just prog -> do
      addFact prog $ Edge "d" "some_other_node"
      addFacts prog [ Edge "e" "f"
                    , Edge "f" "g"
                    , Edge "f" "h"
                    , Edge "g" "some_other_node"
                    ]
      run prog
      -- NOTE: change type param to fetch different relations
      results :: [Reachable] <- getFacts prog
      traverse_ print results

