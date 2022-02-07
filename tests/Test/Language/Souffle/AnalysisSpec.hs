{-# LANGUAGE DataKinds, TypeFamilies, DeriveGeneric #-}

module Test.Language.Souffle.AnalysisSpec
  ( module Test.Language.Souffle.AnalysisSpec
  ) where

import Test.Hspec
import Data.Profunctor
import GHC.Generics
import Control.Monad.IO.Class
import Language.Souffle.Analysis
import Language.Souffle.Interpreted as Souffle

data Path = Path

data Edge = Edge String String
  deriving (Eq, Show, Generic)

data Reachable = Reachable String String
  deriving (Eq, Show, Generic)

instance Souffle.Program Path where
  type ProgramFacts Path = [Edge, Reachable]
  programName = const "path"

instance Souffle.Fact Edge where
  type FactDirection Edge = 'Souffle.InputOutput
  factName = const "edge"

instance Souffle.Fact Reachable where
  type FactDirection Reachable = 'Souffle.Output
  factName = const "reachable"

instance Souffle.Marshal Edge
instance Souffle.Marshal Reachable

data Results = Results [Reachable] [Edge]
  deriving (Eq, Show)

pathAnalysis :: Souffle.Handle Path
             -> Analysis Souffle.SouffleM [Edge] [Reachable]
pathAnalysis h =
  Analysis (Souffle.addFacts h) (Souffle.run h) (Souffle.getFacts h)

-- A little bit silly, but good enough to test different forms of application with
pathAnalysis' :: Souffle.Handle Path
             -> Analysis Souffle.SouffleM [Edge] [Edge]
pathAnalysis' h =
  Analysis (Souffle.addFacts h) (Souffle.run h) (Souffle.getFacts h)

withSouffle :: Souffle.Program a => a -> (Souffle.Handle a -> Souffle.SouffleM ()) -> IO ()
withSouffle prog f = Souffle.runSouffle prog $ \case
  Nothing -> error "Failed to load program"
  Just h -> f h

edges :: [Edge]
edges = [Edge "a" "b", Edge "b" "c", Edge "b" "d", Edge "d" "e"]

spec :: Spec
spec = fdescribe "composing analyses" $ parallel $ do
  it "supports fmap" $ do
    withSouffle Path $ \h -> do
      let analysis = pathAnalysis h
          analysis' = fmap length analysis
      count <- execAnalysis analysis' edges
      liftIO $ count `shouldBe` 8

  describe "analysis used as a profunctor" $ do
    it "supports lmap" $ do
      withSouffle Path $ \h -> do
        let inputs = [("a", "b"), ("b", "c")]
            analysis = pathAnalysis h
            analysis' = lmap (map (uncurry Edge)) analysis
        rs <- execAnalysis analysis' inputs
        liftIO $ rs `shouldBe` [ Reachable "a" "b"
                               , Reachable "a" "c"
                               , Reachable "b" "c"
                               ]

    it "supports rmap" $ do
      withSouffle Path $ \h -> do
        let analysis = pathAnalysis h
            analysis' = rmap length analysis
        count <- execAnalysis analysis' edges
        liftIO $ count `shouldBe` 8

  it "supports applicative composition" $
    withSouffle Path $ \hPath -> do
      let analysis1 = pathAnalysis hPath
          analysis2 = pathAnalysis' hPath
          analysis = Results <$> analysis1 <*> analysis2
          inputs = [Edge "a" "b", Edge "b" "c"]
          reachables = [ Reachable "a" "b"
                       , Reachable "a" "c"
                       , Reachable "b" "c"
                       ]
      results <- execAnalysis analysis inputs
      liftIO $ results `shouldBe` Results reachables inputs

  it "supports semigroupal composition" $ do
    withSouffle Path $ \h -> do
      let analysis = pathAnalysis h
          analysis' = analysis <> analysis
      rs <- execAnalysis analysis' [Edge "a" "b", Edge "b" "c"]
      let results = [ Reachable "a" "b"
                    , Reachable "a" "c"
                    , Reachable "b" "c"
                    ]
          results' = mconcat $ take 2 $ repeat results
      liftIO $ rs `shouldBe` results'

  it "supports mempty" $ do
    withSouffle Path $ \_ -> do
      let analysis :: Analysis Souffle.SouffleM [Edge] [Reachable]
          analysis = mempty
      rs <- execAnalysis analysis [Edge "a" "b", Edge "b" "c"]
      liftIO $ rs `shouldBe` []

  it "supports converting an analysis to a monadic function" $ do
    withSouffle Path $ \h -> do
      let analysis = pathAnalysis h
      rs <- execAnalysis analysis [Edge "a" "b", Edge "b" "c"]
      let results = [ Reachable "a" "b"
                    , Reachable "a" "c"
                    , Reachable "b" "c"
                    ]
      liftIO $ rs `shouldBe` results
