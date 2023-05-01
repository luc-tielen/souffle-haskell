{-# LANGUAGE DataKinds, TypeFamilies, DeriveGeneric, Arrows #-}

module Test.Language.Souffle.AnalysisSpec
  ( module Test.Language.Souffle.AnalysisSpec
  ) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category
import Test.Hspec
import Data.Profunctor
import GHC.Generics
import Control.Monad.IO.Class
import Language.Souffle.Analysis
import qualified Language.Souffle.Interpreted as Souffle

data Path = Path

data Edge = Edge String String
  deriving stock (Eq, Show, Generic)

data Reachable = Reachable String String
  deriving stock (Eq, Show, Generic)

instance Souffle.Program Path where
  type ProgramFacts Path = '[Edge, Reachable]
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
  deriving stock (Eq, Show)

pathAnalysis :: Souffle.Handle Path
             -> Analysis Souffle.SouffleM [Edge] [Reachable]
pathAnalysis h =
  mkAnalysis (Souffle.addFacts h) (Souffle.run h) (Souffle.getFacts h)

-- A little bit silly, but good enough to test different forms of application with
pathAnalysis' :: Souffle.Handle Path
             -> Analysis Souffle.SouffleM [Edge] [Edge]
pathAnalysis' h =
  mkAnalysis (Souffle.addFacts h) (Souffle.run h) (Souffle.getFacts h)

data RoundTrip = RoundTrip

newtype StringFact = StringFact String
  deriving stock (Eq, Show, Generic)

instance Souffle.Program RoundTrip where
  type ProgramFacts RoundTrip = '[StringFact]

  programName = const "round_trip"

instance Souffle.Fact StringFact where
  type FactDirection StringFact = 'Souffle.InputOutput

  factName = const "string_fact"

instance Souffle.Marshal StringFact


roundTripAnalysis :: Souffle.Handle RoundTrip
                  -> Analysis Souffle.SouffleM [Reachable] [StringFact]
roundTripAnalysis h =
  mkAnalysis addFacts (Souffle.run h) (Souffle.getFacts h)
  where
    addFacts rs = do
      Souffle.addFacts h $ map (\(Reachable a _) -> StringFact a) rs

withSouffle :: Souffle.Program a => a -> (Souffle.Handle a -> Souffle.SouffleM ()) -> IO ()
withSouffle prog f = Souffle.runSouffle prog $ \case
  Nothing -> error "Failed to load program"
  Just h -> f h

edges :: [Edge]
edges = [Edge "a" "b", Edge "b" "c", Edge "b" "d", Edge "d" "e"]

spec :: Spec
spec = describe "composing analyses" $ parallel $ do
  it "supports fmap" $ do
    withSouffle Path $ \h -> do
      let analysis = pathAnalysis h
          analysis' = fmap length analysis
      count <- execAnalysis analysis' edges
      liftIO $ count `shouldBe` 8

  describe "analysis used as a profunctor" $ parallel $ do
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
          results' = mconcat $ replicate 2 results
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

  describe "analysis used as a category" $ parallel $ do
    it "supports 'id'" $ do
      withSouffle Path $ \_ -> do
        let analysis :: Analysis Souffle.SouffleM [Edge] [Edge]
            analysis = id
        edges' <- execAnalysis analysis edges
        liftIO $ edges' `shouldBe` edges

    it "supports sequential composition using (.)" $ do
      withSouffle Path $ \h -> do
        let reachableToFlippedEdge (Reachable a b) = Edge b a
            analysis1 = pathAnalysis h
            analysis2 = lmap (map reachableToFlippedEdge) $ pathAnalysis h
        rs <- execAnalysis (analysis2 . analysis1) [Edge "a" "b", Edge "b" "c"]
        let results = [ Reachable "a" "a"
                      , Reachable "a" "b"
                      , Reachable "a" "c"
                      , Reachable "b" "a"
                      , Reachable "b" "b"
                      , Reachable "b" "c"
                      , Reachable "c" "a"
                      , Reachable "c" "b"
                      , Reachable "c" "c"
                      ]
        liftIO $ rs `shouldBe` results

  describe "analysis used as an arrow" $ parallel $ do
    it "supports 'arr'" $ do
      withSouffle Path $ \_ -> do
        let analysis :: Analysis Souffle.SouffleM Int Int
            analysis = arr (+1)
        result1 <- execAnalysis analysis 41
        result2 <- execAnalysis (arr id) 41
        liftIO $ result1 `shouldBe` 42
        liftIO $ result2 `shouldBe` 41

    it "supports 'first'" $ do
      withSouffle Path $ \_ -> do
        let analysis :: Analysis Souffle.SouffleM (Int, Bool) (Int, Bool)
            analysis = first (arr (+1))
            input = (41, True)
        result <- execAnalysis analysis input
        liftIO $ result `shouldBe` (42, True)

    it "supports 'second'" $ do
      withSouffle Path $ \_ -> do
        let analysis :: Analysis Souffle.SouffleM (Bool, Int) (Bool, Int)
            analysis = second (arr (+1))
            input = (True, 41)
        result <- execAnalysis analysis input
        liftIO $ result `shouldBe` (True, 42)

    it "supports (***)" $ do
      withSouffle Path $ \_ -> do
        let analysis :: Analysis Souffle.SouffleM (Bool, Int) (Bool, Int)
            analysis = arr not *** arr (+1)
            input = (True, 41)
        result <- execAnalysis analysis input
        liftIO $ result `shouldBe` (False, 42)

    it "supports (&&&)" $ do
      withSouffle Path $ \_ -> do
        let analysis :: Analysis Souffle.SouffleM Int (Bool, Int)
            analysis = arr (== 1000) &&& arr (+1)
            input = 41
        result <- execAnalysis analysis input
        liftIO $ result `shouldBe` (False, 42)

    it "supports arrow notation" $ do
      withSouffle Path $ \h -> do
        liftIO $ withSouffle RoundTrip $ \h' -> do
          let arrowAnalysis = proc es -> do
                rs <- pathAnalysis h -< es
                strs <- roundTripAnalysis h' -< rs
                returnA -< strs
          result <- execAnalysis arrowAnalysis edges
          liftIO $ result `shouldBe` [ StringFact "a"
                                     , StringFact "b"
                                     , StringFact "d"
                                     ]

    it "supports case expressions in arrow notation" $ do
      withSouffle Path $ \h -> do
        let analysis =  proc es -> do
              rs <- pathAnalysis h -< es
              case rs of
                [] -> returnA -< []
                rs' -> returnA -< take 2 rs'
        result <- execAnalysis analysis edges
        let expected = [ Reachable "a" "b", Reachable "a" "c" ]
        liftIO $ result `shouldBe` expected

