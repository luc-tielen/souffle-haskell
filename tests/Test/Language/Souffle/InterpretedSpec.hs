
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, DeriveGeneric #-}

module Test.Language.Souffle.InterpretedSpec
  ( module Test.Language.Souffle.InterpretedSpec
  ) where

import Test.Hspec
import GHC.Generics
import Data.Maybe
import Control.Monad.IO.Class (liftIO)
import System.Directory
import System.IO.Temp
import qualified Data.Array as A
import qualified Data.Vector as V
import qualified Language.Souffle.Interpreted as Souffle


data Path = Path

data PathNoInput = PathNoInput  -- doesn't mark edge as an input

data BadPath = BadPath

data Edge = Edge String String
  deriving (Eq, Show, Generic)

data Reachable = Reachable String String
  deriving (Eq, Show, Generic)

instance Souffle.Fact Edge where
  type FactDirection Edge = 'Souffle.InputOutput
  factName = const "edge"

instance Souffle.Fact Reachable where
  type FactDirection Reachable = 'Souffle.Output
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

getTestTemporaryDirectory :: IO FilePath
getTestTemporaryDirectory = do
  tmpDir <- getCanonicalTemporaryDirectory
  createTempDirectory tmpDir "souffle-haskell-test"

spec :: Spec
spec = describe "Souffle API" $ parallel $ do
  describe "init" $ parallel $ do
    it "returns nothing in case it cannot load a souffle program" $ do
      prog <- Souffle.runSouffle BadPath pure
      isJust prog `shouldBe` False

    it "returns just the program in case it can load a souffle program" $ do
      prog <- Souffle.runSouffle Path pure
      isJust prog `shouldBe` True

  describe "getFacts" $ parallel $ do
    it "can retrieve facts as a list" $ do
      (edges, reachables) <- Souffle.runSouffle PathNoInput $ \handle -> do
        let prog = fromJust handle
        Souffle.run prog
        es <- Souffle.getFacts prog
        rs <- Souffle.getFacts prog
        pure (es , rs)
      edges `shouldBe` [Edge "a" "b", Edge "b" "c"]
      reachables `shouldBe` [Reachable "a" "b", Reachable "a" "c", Reachable "b" "c"]

    it "can retrieve facts as a vector" $ do
      (edges, reachables) <- Souffle.runSouffle PathNoInput $ \handle -> do
        let prog = fromJust handle
        Souffle.run prog
        es <- Souffle.getFacts prog
        rs <- Souffle.getFacts prog
        pure (es , rs)
      edges `shouldBe` V.fromList [Edge "a" "b", Edge "b" "c"]
      reachables `shouldBe` V.fromList [Reachable "a" "b", Reachable "a" "c", Reachable "b" "c"]

    it "can retrieve facts as an array" $ do
      (edges, reachables) <- Souffle.runSouffle PathNoInput $ \handle -> do
        let prog = fromJust handle
        Souffle.run prog
        es <- Souffle.getFacts prog
        rs <- Souffle.getFacts prog
        pure (es , rs)
      edges `shouldBe` A.listArray (0 :: Int, 1) [Edge "a" "b", Edge "b" "c"]
      reachables `shouldBe` A.listArray (0 :: Int, 2) [Reachable "a" "b", Reachable "a" "c", Reachable "b" "c"]

    it "returns no facts in case program hasn't run yet" $ do
      edges <- Souffle.runSouffle Path $ Souffle.getFacts . fromJust
      edges `shouldBe` ([] :: [Edge])

    it "can retrieve facts from custom output directory" $ do
      cfg <- Souffle.defaultConfig
      tmp <- getTestTemporaryDirectory
      let cfg' = cfg { Souffle.cfgOutputDir = Just tmp }
      (edges, reachables) <- Souffle.runSouffleWith cfg' PathNoInput $ \handle -> do
        let prog = fromJust handle
        Souffle.run prog
        es <- Souffle.getFacts prog
        rs <- Souffle.getFacts prog
        pure (es , rs)
      edges `shouldBe` V.fromList [Edge "a" "b", Edge "b" "c"]
      reachables `shouldBe` V.fromList [Reachable "a" "b", Reachable "a" "c", Reachable "b" "c"]
      outputDirExist <- doesDirectoryExist tmp
      outputDirExist `shouldBe` True  -- should not be automatically cleaned up

  describe "addFact" $ parallel $ do
    it "adds a fact" $ do
      edges <- Souffle.runSouffle Path $ \handle -> do
        let prog = fromJust handle
        Souffle.addFact prog $ Edge "e" "f"
        Souffle.run prog
        Souffle.getFacts prog
      edges `shouldBe` [Edge "a" "b", Edge "b" "c", Edge "e" "f"]

    it "adds a fact to a custom input directory" $ do
      cfg <- Souffle.defaultConfig
      tmp <- getTestTemporaryDirectory
      let cfg' = cfg { Souffle.cfgFactDir = Just tmp }
      Souffle.runSouffleWith cfg' Path $ \handle -> do
        let prog = fromJust handle
        Souffle.addFact prog $ Edge "e" "f"
        Souffle.run prog
        edges <- Souffle.getFacts prog
        liftIO $ do
          entries <- listDirectory tmp
          edges `shouldBe` [Edge "a" "b", Edge "b" "c", Edge "e" "f"]
          length entries `shouldNotBe` 0

  describe "addFacts" $ parallel $
    it "can add multiple facts at once" $ do
      edges <- Souffle.runSouffle Path $ \handle -> do
        let prog = fromJust handle
        Souffle.addFacts prog [Edge "e" "f", Edge "f" "g"]
        Souffle.run prog
        Souffle.getFacts prog
      edges `shouldBe` [Edge "a" "b", Edge "b" "c", Edge "e" "f", Edge "f" "g"]

  describe "run" $ parallel $ do
    it "is OK to run a program multiple times" $ do
      edges <- Souffle.runSouffle PathNoInput $ \handle -> do
        let prog = fromJust handle
        Souffle.run prog
        Souffle.run prog
        Souffle.getFacts prog
      edges `shouldBe` [Reachable "a" "b", Reachable "a" "c", Reachable "b" "c"]

    it "discovers new facts after running with new facts" $ do
      (reachablesBefore, reachablesAfter) <- Souffle.runSouffle Path $ \handle -> do
        let prog = fromJust handle
        Souffle.addFacts prog [Edge "c" "d"]
        Souffle.run prog
        rs1 <- Souffle.getFacts prog
        Souffle.addFacts prog [Edge "b" "e"]
        Souffle.run prog
        rs2 <- Souffle.getFacts prog
        pure (rs1, rs2)
      reachablesBefore `shouldBe`
        [ Reachable "a" "b", Reachable "a" "c", Reachable "a" "d"
        , Reachable "b" "c", Reachable "b" "d", Reachable "c" "d" ]
      reachablesAfter `shouldBe`
        [ Reachable "a" "b", Reachable "a" "c", Reachable "a" "d"
        , Reachable "a" "e", Reachable "b" "c", Reachable "b" "d"
        , Reachable "b" "e", Reachable "c" "d" ]

    it "saves stdout and stderr output after run" $ do
      (stdout, stderr) <- Souffle.runSouffle PathNoInput $ \handle -> do
        let prog = fromJust handle
        Souffle.run prog
        out <- Souffle.souffleStdOut prog
        err <- Souffle.souffleStdErr prog
        pure (out, err)
      stdout `shouldBe` Just ""
      stderr `shouldBe` Just ""

  describe "configuring number of cores" $ parallel $
    it "is possible to configure number of cores" $ do
      results <- Souffle.runSouffle Path $ \handle -> do
        let prog = fromJust handle
        numCpus1 <- Souffle.getNumThreads prog
        Souffle.setNumThreads prog 4
        numCpus2 <- Souffle.getNumThreads prog
        Souffle.setNumThreads prog 2
        numCpus3 <- Souffle.getNumThreads prog
        pure (numCpus1, numCpus2, numCpus3)
      results `shouldBe` (1, 4, 2)

  describe "findFact" $ parallel $ do
    it "returns Nothing in case no matching fact was found" $ do
      (edge, reachable) <- Souffle.runSouffle PathNoInput $ \handle -> do
        let prog = fromJust handle
        Souffle.run prog
        e <- Souffle.findFact prog $ Edge "c" "d"
        r <- Souffle.findFact prog $ Reachable "d" "e"
        pure (e, r)
      edge `shouldBe` Nothing
      reachable `shouldBe` Nothing

    it "returns Just the fact in case matching fact was found" $ do
      (edge, reachable) <- Souffle.runSouffle PathNoInput $ \handle -> do
        let prog = fromJust handle
        Souffle.run prog
        e <- Souffle.findFact prog $ Edge "a" "b"
        r <- Souffle.findFact prog $ Reachable "a" "c"
        pure (e, r)
      edge `shouldBe` Just (Edge "a" "b")
      reachable `shouldBe` Just (Reachable "a" "c")

  describe "Semigroup and Monoid instances" $ parallel $ do
    it "combines Souffle actions into one using (<>)" $ do
      edges <- Souffle.runSouffle Path $ \handle -> do
        let prog = fromJust handle
            action1 = Souffle.addFact prog $ Edge "e" "f"
            action2 = Souffle.addFact prog $ Edge "f" "g"
            action = action1 <> action2
        action
        Souffle.run prog
        Souffle.getFacts prog
      edges `shouldBe` [ Edge "a" "b", Edge "b" "c"
                       , Edge "e" "f", Edge "f" "g"
                       ]

    it "supports mempty" $ do
      edges <- Souffle.runSouffle Path $ \handle -> do
        let prog = fromJust handle
            action = Souffle.addFact prog $ Edge "e" "f"
            action' = action <> mempty
        action'
        Souffle.run prog
        Souffle.getFacts prog
      edges `shouldBe` [Edge "a" "b", Edge "b" "c", Edge "e" "f"]

    it "supports foldMap" $ do
      edges <- Souffle.runSouffle Path $ \handle -> do
        let prog = fromJust handle
            fact1 = Edge "e" "f"
            fact2 = Edge "f" "g"
            action = foldMap (Souffle.addFact prog) [fact1, fact2]
        action
        Souffle.run prog
        Souffle.getFacts prog
      edges `shouldBe` [ Edge "a" "b", Edge "b" "c"
                       , Edge "e" "f", Edge "f" "g"
                       ]

