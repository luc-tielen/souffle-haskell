
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, DataKinds, TypeFamilies #-}

module Main ( main ) where

import Prelude hiding ( init )
import Data.Foldable ( traverse_ )
import Language.Souffle.TH
import Language.Souffle

embedProgram "path.cpp"


data Path = Path

data Edge = Edge String String
  deriving (Eq, Show)

data Reachable = Reachable String String
  deriving (Eq, Show)

instance Program Path where
  type ProgramFacts Path = [Edge, Reachable]
  programName = const "path"

instance Fact Edge where
  factName = const "edge"

instance Fact Reachable where
  factName = const "reachable"


instance Marshal Edge where
  push (Edge x y) = do
    push x
    push y
  pop = Edge <$> pop <*> pop

instance Marshal Reachable where
  push (Reachable x y) = do
    push x
    push y
  pop = Reachable <$> pop <*> pop


main :: IO ()
main = do
  maybeProgram <- init Path
  case maybeProgram of
    Nothing -> putStrLn "Failed to load program."
    Just prog -> do
      addFact prog $ Edge "d" "i"
      addFacts prog [ Edge "e" "f"
                    , Edge "f" "g"
                    , Edge "f" "h"
                    , Edge "g" "i"
                    ]
      run prog
      -- NOTE: change type param to fetch different relations
      results :: [Reachable] <- getFacts prog
      traverse_ print results

