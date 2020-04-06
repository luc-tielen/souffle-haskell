{-# LANGUAGE DataKinds, TypeFamilies, DeriveGeneric #-}
module Test.InterpreterSpec (spec) where

import Prelude hiding (init)
import GHC.Generics
import Language.Souffle.Interpreter
import Test.Hspec

spec :: Spec
spec = do
  it "Interpreter" $ do
    runSouffle $ do
      Just handle <- init Path
      addFact handle $ Reachable "x" "y"
      run handle
      cleanUp handle

data Path = Path

data Edge = Edge String String
  deriving (Eq, Show, Generic)

data Reachable = Reachable String String
  deriving (Eq, Show, Generic)

instance Program Path where
  type ProgramFacts Path = [Edge, Reachable]
  programName = const "path"

instance Fact Edge where
  factName = const "edge"

instance Fact Reachable where
  factName = const "reachable"

instance Marshal Edge
instance Marshal Reachable
