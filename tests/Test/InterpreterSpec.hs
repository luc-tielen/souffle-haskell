{-# LANGUAGE DataKinds, TypeFamilies, DeriveGeneric, ScopedTypeVariables #-}
module Test.InterpreterSpec (spec) where

import GHC.Generics
import Language.Souffle.Interpreter as I
import Language.Souffle as S
import Test.Hspec

spec :: Spec
spec = do
  it "Interpreter" $ do
    runSouffleM $ do
      Just handle <- I.init Path
      I.run handle
      I.addFact handle $ Reachable "x" "y"
      I.cleanUp handle

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
