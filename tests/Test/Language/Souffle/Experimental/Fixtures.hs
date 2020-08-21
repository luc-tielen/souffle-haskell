
{-# LANGUAGE DataKinds, TypeFamilies, DeriveGeneric, DeriveAnyClass #-}

module Test.Language.Souffle.Experimental.Fixtures
  ( module Test.Language.Souffle.Experimental.Fixtures
  ) where

import GHC.Generics
import Language.Souffle.Class

data CompiledProgram = CompiledProgram

instance Program CompiledProgram where
  type ProgramFacts CompiledProgram = [Edge, Reachable]
  programName = const "compiledprogram"

data Edge = Edge String String
  deriving (Generic, Marshal)

data Reachable = Reachable String String
  deriving (Eq, Show, Generic, Marshal)

instance Fact Edge where
  type FactDirection Edge = 'Input
  factName = const "edge"

instance Fact Reachable where
  type FactDirection Reachable = 'Output
  factName = const "reachable"

