
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, DeriveGeneric #-}

module Test.Language.Souffle.InterpretedSpec
  ( module Test.Language.Souffle.InterpretedSpec
  ) where

import Test.Hspec
import GHC.Generics
import Data.Maybe
import qualified Language.Souffle.Interpreter as Souffle


data Path = Path

data PathNoInput = PathNoInput  -- doesn't mark edge as an input

data BadPath = BadPath

data Edge = Edge String String
  deriving (Eq, Show, Generic)

data Reachable = Reachable String String
  deriving (Eq, Show, Generic)

instance Souffle.Fact Edge where
  factName = const "edge"

instance Souffle.Fact Reachable where
  factName = const "reachable"

instance Souffle.Marshal Edge
instance Souffle.Marshal Reachable

instance Souffle.Program Path where
  type ProgramFacts Path = [Edge, Reachable]
  programName = const "path"

instance Souffle.Program PathNoInput where
  type ProgramFacts PathNoInput = [Edge, Reachable]
  programName = const "path_no_input"

instance Souffle.Program BadPath where
  type ProgramFacts BadPath = [Edge, Reachable]
  programName = const "bad_path"


spec :: Spec
spec = describe "Souffle API" $ parallel $ do
  describe "init" $ parallel $ do
    it "returns nothing if it cannot load a souffle program" $ do
      prog <- Souffle.runSouffle (Souffle.init BadPath)
      isJust prog `shouldBe` False

    it "returns just the program if it can load a souffle program" $ do
      prog <- Souffle.runSouffle $ do
        Just handle <- Souffle.init Path
        Souffle.cleanup handle
        pure $ Just handle
      isJust prog `shouldBe` True

  describe "getFacts" $ parallel $ do
    it "can retrieve facts as a list" $ do
      (edges, reachables) <- Souffle.runSouffle $ do
        prog <- fromJust <$> Souffle.init PathNoInput
        Souffle.run prog
        es <- Souffle.getFacts prog
        rs <- Souffle.getFacts prog
        Souffle.cleanup prog
        pure (es , rs)
      edges `shouldBe` [Edge "a" "b", Edge "b" "c"]
      reachables `shouldBe` [Reachable "a" "b", Reachable "a" "c", Reachable "b" "c"]

    it "returns no facts if program hasn't run yet" $ do
      edges <- Souffle.runSouffle $ do
        prog <- fromJust <$> Souffle.init Path
        results <- Souffle.getFacts prog
        Souffle.cleanup prog
        pure results
      edges `shouldBe` ([] :: [Edge])

  describe "addFact" $ parallel $ do
    it "adds a fact" $ do
      edges <- Souffle.runSouffle $ do
        prog <- fromJust <$> Souffle.init Path
        Souffle.addFact prog $ Edge "e" "f"
        Souffle.run prog
        Souffle.getFacts prog
      edges `shouldBe` [Edge "a" "b", Edge "b" "c", Edge "e" "f"]

    -- NOTE: this is different compared to compiled version (bug in Souffle?)
    it "can not add a fact if it is not marked as input" $ do
      reachables <- Souffle.runSouffle $ do
        prog <- fromJust <$> Souffle.init PathNoInput
        Souffle.addFact prog $ Reachable "e" "f"
        Souffle.run prog
        Souffle.getFacts prog
      reachables `shouldBe`
        [ Reachable "a" "b", Reachable "a" "c", Reachable "b" "c" ]

  describe "addFacts" $ parallel $
    it "can add multiple facts at once" $ do
      edges <- Souffle.runSouffle $ do
        prog <- fromJust <$> Souffle.init Path
        Souffle.addFacts prog [Edge "e" "f", Edge "f" "g"]
        Souffle.run prog
        Souffle.getFacts prog
      edges `shouldBe` [Edge "a" "b", Edge "b" "c", Edge "e" "f", Edge "f" "g"]

  describe "run" $ parallel $ do
    it "is OK to run a program multiple times" $ do
      edges <- Souffle.runSouffle $ do
        prog <- fromJust <$> Souffle.init PathNoInput
        Souffle.run prog
        Souffle.run prog
        facts <- Souffle.getFacts prog
        Souffle.cleanup prog
        pure facts
      edges `shouldBe` [Reachable "a" "b", Reachable "a" "c", Reachable "b" "c"]

    it "discovers new facts after running with new facts" $ do
      (reachablesBefore, reachablesAfter) <- Souffle.runSouffle $ do
        prog <- fromJust <$> Souffle.init Path
        Souffle.addFacts prog [Edge "c" "d"]
        Souffle.run prog
        rs1 <- Souffle.getFacts prog
        Souffle.addFacts prog [Edge "b" "e"]
        Souffle.run prog
        rs2 <- Souffle.getFacts prog
        Souffle.cleanup prog
        pure (rs1, rs2)
      reachablesBefore `shouldBe`
        [ Reachable "a" "b", Reachable "a" "c", Reachable "a" "d"
        , Reachable "b" "c", Reachable "b" "d", Reachable "c" "d" ]
      reachablesAfter `shouldBe`
        [ Reachable "a" "b", Reachable "a" "c", Reachable "a" "d"
        , Reachable "a" "e", Reachable "b" "c", Reachable "b" "d"
        , Reachable "b" "e", Reachable "c" "d" ]

  describe "configuring number of cores" $ parallel $
    it "is possible to configure number of cores" $ do
      results <- Souffle.runSouffle $ do
        prog <- fromJust <$> Souffle.init Path
        numCpus1 <- Souffle.getNumThreads prog
        Souffle.setNumThreads prog 4
        numCpus2 <- Souffle.getNumThreads prog
        Souffle.setNumThreads prog 2
        numCpus3 <- Souffle.getNumThreads prog
        pure (numCpus1, numCpus2, numCpus3)
      results `shouldBe` (1, 4, 2)

  describe "findFact" $ parallel $ do
    it "returns Nothing if no matching fact was found" $ do
      (edge, reachable) <- Souffle.runSouffle $ do
        prog <- fromJust <$> Souffle.init PathNoInput
        Souffle.run prog
        e <- Souffle.findFact prog $ Edge "c" "d"
        r <- Souffle.findFact prog $ Reachable "d" "e"
        pure (e, r)
      edge `shouldBe` Nothing
      reachable `shouldBe` Nothing

    it "returns Just the fact if matching fact was found" $ do
      (edge, reachable) <- Souffle.runSouffle $ do
        prog <- fromJust <$> Souffle.init PathNoInput
        Souffle.run prog
        e <- Souffle.findFact prog $ Edge "a" "b"
        r <- Souffle.findFact prog $ Reachable "a" "c"
        pure (e, r)
      edge `shouldBe` Just (Edge "a" "b")
      reachable `shouldBe` Just (Reachable "a" "c")
