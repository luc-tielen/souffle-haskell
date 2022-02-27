{-# LANGUAGE UndecidableInstances, DataKinds, DeriveGeneric, DeriveAnyClass, DerivingVia #-}

module Test.Language.Souffle.DerivingViaSpec
  ( module Test.Language.Souffle.DerivingViaSpec
  ) where

import Test.Hspec
import GHC.Generics
import Data.Maybe
import qualified Language.Souffle.Interpreted as Souffle


data Path = Path
  deriving Souffle.Program
  via Souffle.ProgramOptions Path "path" '[Edge, Reachable]

data Edge = Edge String String
  deriving stock (Eq, Show, Generic)
  deriving anyclass Souffle.Marshal
  deriving Souffle.Fact
  via Souffle.FactOptions Edge "edge" 'Souffle.InputOutput

data Reachable = Reachable String String
  deriving stock (Eq, Show, Generic)
  deriving anyclass Souffle.Marshal
  deriving Souffle.Fact
  via Souffle.FactOptions Reachable "reachable" 'Souffle.Output


spec :: Spec
spec = describe "Souffle DerivingVia-style API" $ do
  it "can get and put facts from souffle" $ do

    edges <- Souffle.runSouffle Path $ \handle -> do
      let prog = fromJust handle
      Souffle.addFacts prog [Edge "e" "f", Edge "f" "g"]
      Souffle.run prog
      Souffle.getFacts prog
    edges `shouldBe` [Edge "a" "b", Edge "b" "c", Edge "e" "f", Edge "f" "g"]

