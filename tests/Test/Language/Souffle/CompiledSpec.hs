{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, DataKinds #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}

module Test.Language.Souffle.CompiledSpec
  ( module Test.Language.Souffle.CompiledSpec
  ) where

import Test.Hspec
import GHC.Generics
import Data.Maybe
import qualified Data.Vector as V
import qualified Language.Souffle as Souffle

data Path = Path

data Edge = Edge String String
  deriving (Eq, Show, Generic)

data Reachable = Reachable String String
  deriving (Eq, Show, Generic)

instance Souffle.Program Path where
  type ProgramFacts Path = [Edge, Reachable]
  programName = const "path"

instance Souffle.Fact Edge where
  factName = const "edge"

instance Souffle.Fact Reachable where
  factName = const "reachable"

instance Souffle.Marshal Edge
instance Souffle.Marshal Reachable


data BadPath = BadPath

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
      prog <- Souffle.runSouffle (Souffle.init Path)
      isJust prog `shouldBe` True

  describe "getFacts" $ parallel $ do
    it "can retrieve facts as a list" $ do
      (edges, reachables) <- Souffle.runSouffle $ do
        prog <- fromJust <$> Souffle.init Path
        Souffle.run prog
        es <- Souffle.getFacts prog
        rs <- Souffle.getFacts prog
        pure (es , rs)
      edges `shouldBe` [Edge "b" "c", Edge "a" "b"]
      reachables `shouldBe` [Reachable "b" "c", Reachable "a" "c", Reachable "a" "b"]

    it "can retrieve facts as a vector" $ do
      (edges, reachables) <- Souffle.runSouffle $ do
        prog <- fromJust <$> Souffle.init Path
        Souffle.run prog
        es <- Souffle.getFacts prog
        rs <- Souffle.getFacts prog
        pure (es , rs)
      edges `shouldBe` V.fromList [Edge "a" "b", Edge "b" "c"]
      reachables `shouldBe` V.fromList [Reachable "a" "b", Reachable "a" "c", Reachable "b" "c"]

    it "returns no facts if program hasn't run yet" $ do
      edges <- Souffle.runSouffle $ do
        prog <- fromJust <$> Souffle.init Path
        Souffle.getFacts prog
      edges `shouldBe` ([] :: [Edge])

  describe "addFact" $ parallel $ do
    it "adds a fact" $ do
      (edgesBefore, edgesAfter) <- Souffle.runSouffle $ do
        prog <- fromJust <$> Souffle.init Path
        Souffle.run prog
        es1 <- Souffle.getFacts prog
        Souffle.addFact prog $ Edge "e" "f"
        Souffle.run prog
        es2 <- Souffle.getFacts prog
        pure (es1, es2)
      edgesBefore `shouldBe` [Edge "b" "c", Edge "a" "b"]
      edgesAfter `shouldBe` [Edge "e" "f", Edge "b" "c", Edge "a" "b"]

    -- NOTE: this is different compared to interpreted version (bug in Souffle?)
    it "can add a fact even if it is marked as output" $ do
      reachables <- Souffle.runSouffle $ do
        prog <- fromJust <$> Souffle.init Path
        Souffle.addFact prog $ Reachable "e" "f"
        Souffle.run prog
        Souffle.getFacts prog
      reachables `shouldBe` [ Reachable "e" "f", Reachable "b" "c"
                            , Reachable "a" "c", Reachable "a" "b" ]

  describe "addFacts" $ parallel $
    it "can add multiple facts at once" $ do
      (edgesBefore, edgesAfter) <- Souffle.runSouffle $ do
        prog <- fromJust <$> Souffle.init Path
        Souffle.run prog
        es1 <- Souffle.getFacts prog
        Souffle.addFacts prog [Edge "e" "f", Edge "f" "g"]
        Souffle.run prog
        es2 <- Souffle.getFacts prog
        pure (es1, es2)
      edgesBefore `shouldBe` [Edge "b" "c", Edge "a" "b"]
      edgesAfter `shouldBe` [Edge "f" "g", Edge "e" "f", Edge "b" "c", Edge "a" "b"]

  describe "run" $ parallel $ do
    it "is OK to run a program multiple times" $ do
      edges <- Souffle.runSouffle $ do
        prog <- fromJust <$> Souffle.init Path
        Souffle.run prog
        Souffle.run prog
        Souffle.getFacts prog
      edges `shouldBe` [Edge "b" "c", Edge "a" "b"]

    it "discovers new facts after running with new facts" $ do
      (reachablesBefore, reachablesAfter) <- Souffle.runSouffle $ do
        prog <- fromJust <$> Souffle.init Path
        Souffle.run prog
        rs1 <- Souffle.getFacts prog
        Souffle.addFacts prog [Edge "e" "f", Edge "f" "g"]
        Souffle.run prog
        rs2 <- Souffle.getFacts prog
        pure (rs1, rs2)
      reachablesBefore `shouldBe` [Reachable "b" "c", Reachable "a" "c", Reachable "a" "b"]
      reachablesAfter `shouldBe` [ Reachable "f" "g", Reachable "e" "g", Reachable "e" "f"
                                 , Reachable "b" "c",Reachable "a" "c", Reachable "a" "b" ]

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
        prog <- fromJust <$> Souffle.init Path
        Souffle.run prog
        e <- Souffle.findFact prog $ Edge "c" "d"
        r <- Souffle.findFact prog $ Reachable "d" "e"
        pure (e, r)
      edge `shouldBe` Nothing
      reachable `shouldBe` Nothing

    it "returns Just the fact if matching fact was found" $ do
      (edge, reachable) <- Souffle.runSouffle $ do
        prog <- fromJust <$> Souffle.init Path
        Souffle.run prog
        e <- Souffle.findFact prog $ Edge "a" "b"
        r <- Souffle.findFact prog $ Reachable "a" "c"
        pure (e, r)
      edge `shouldBe` Just (Edge "a" "b")
      reachable `shouldBe` Just (Reachable "a" "c")

  -- TODO writeFiles / loadFiles
