{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
module Main ( main ) where

import Criterion.Main
import qualified Language.Souffle.Compiled as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics
import Data.Word
import Data.Int
import Control.Monad
import Control.Monad.IO.Class
import Control.DeepSeq


data Benchmarks = Benchmarks

data NumbersFact
  = NumbersFact Word32 Int32 Float
  deriving (Generic, NFData)

data StringsFact
  = StringsFact Word32 T.Text Int32 Float
  deriving (Generic, NFData)

newtype FromDatalogFact
  = FromDatalogFact Int32
  deriving (Generic, NFData)

data FromDatalogStringFact
  = FromDatalogStringFact Int32 T.Text
  deriving (Generic, NFData)

instance S.Program Benchmarks where
  type ProgramFacts Benchmarks =
      '[NumbersFact, StringsFact, FromDatalogFact, FromDatalogStringFact]
  programName = const "bench"

instance S.Fact NumbersFact where
  type FactDirection NumbersFact = 'S.InputOutput
  factName = const "numbers_fact"

instance S.Fact StringsFact where
  type FactDirection StringsFact = 'S.InputOutput
  factName = const "strings_fact"

instance S.Fact FromDatalogFact where
  type FactDirection FromDatalogFact = 'S.InputOutput
  factName = const "from_datalog_fact"

instance S.Fact FromDatalogStringFact where
  type FactDirection FromDatalogStringFact = 'S.InputOutput
  factName = const "from_datalog_string_fact"

instance S.Marshal NumbersFact
instance S.Marshal StringsFact
instance S.Marshal FromDatalogFact
instance S.Marshal FromDatalogStringFact


-- TODO: fix cases with larger numbers (crashes due to large memory allocations?)
main :: IO ()
main = defaultMain
     $ roundTripBenchmarks
    ++ serializationBenchmarks
    ++ deserializationBenchmarks

roundTripBenchmarks :: [Benchmark]
roundTripBenchmarks =
  [ bgroup "round trip facts (without strings)"
    [ bench "1"      $ nfIO $ roundTrip $ mkVec 1
    , bench "10"     $ nfIO $ roundTrip $ mkVec 10
    , bench "100"    $ nfIO $ roundTrip $ mkVec 100
    , bench "1000"   $ nfIO $ roundTrip $ mkVec 1000
    , bench "10000"  $ nfIO $ roundTrip $ mkVec 10000
    --, bench "100000" $ nfIO $ roundTrip $ mkVec 100000
    ]
  , bgroup "round trip facts (with strings)"
    [ bench "1"      $ nfIO $ roundTrip $ mkVecStr 1
    , bench "10"     $ nfIO $ roundTrip $ mkVecStr 10
    , bench "100"    $ nfIO $ roundTrip $ mkVecStr 100
    , bench "1000"   $ nfIO $ roundTrip $ mkVecStr 1000
    --, bench "10000"  $ nfIO $ roundTrip $ mkVecStr 10000
    --, bench "100000" $ nfIO $ roundTrip $ mkVecStr 100000
    ]
  , bgroup "round trip facts (with long strings)"
    [ bench "1"      $ nfIO $ roundTrip $ mkVecLongStr 1
    , bench "10"     $ nfIO $ roundTrip $ mkVecLongStr 10
    , bench "100"    $ nfIO $ roundTrip $ mkVecLongStr 100
    , bench "1000"   $ nfIO $ roundTrip $ mkVecLongStr 1000
    --, bench "10000"  $ nfIO $ roundTrip $ mkVecLongStr 10000
    --, bench "100000" $ nfIO $ roundTrip $ mkVecLongStr 100000
    ]
  ]
  where mkVec count = V.generate count $ \i -> NumbersFact (fromIntegral i) (-42) 3.14
        mkVecStr count = V.generate count $ \i -> StringsFact (fromIntegral i) "abcdef" (-42) 3.14
        mkVecLongStr count = V.generate count $ \i -> StringsFact (fromIntegral i) (T.replicate 10 "abcdef") (-42) 3.14

roundTrip :: (S.ContainsInputFact Benchmarks a, S.ContainsOutputFact Benchmarks a, S.Fact a, S.Submit a)
          => V.Vector a -> IO (V.Vector a)
roundTrip vec = S.runSouffle Benchmarks $ \case
  Nothing -> do
    liftIO $ print "Failed to load roundtrip benchmarks!"
    pure V.empty
  Just prog -> do
    S.addFacts prog vec
    -- No run needed
    S.getFacts prog


serializeNumbers :: Int -> IO ()
serializeNumbers iterationCount = S.runSouffle Benchmarks $ \case
  Nothing -> liftIO $ print "Failed to load serialize benchmarks!"
  Just prog ->
    replicateM_ iterationCount $ S.addFacts prog vec
    -- No run needed
  where vec = V.generate 100 $ \i -> NumbersFact (fromIntegral i) (-42) 3.14

deserializeNumbers :: Int -> IO ()
deserializeNumbers iterationCount = S.runSouffle Benchmarks $ \case
  Nothing -> liftIO $ print "Failed to load deserialize benchmarks!"
  Just prog -> do
    S.run prog
    replicateM_ iterationCount $ do
      fs <- S.getFacts prog
      pure (fs :: V.Vector FromDatalogFact)

serializeWithStrings :: Int -> IO ()
serializeWithStrings iterationCount = S.runSouffle Benchmarks $ \case
  Nothing -> liftIO $ print "Failed to load serialize benchmarks!"
  Just prog ->
    replicateM_ iterationCount $ S.addFacts prog vec
    -- No run needed
  where vec = V.generate 100 $ \i -> StringsFact (fromIntegral i) "abcdef" (-42) 3.14

deserializeWithStrings :: Int -> IO ()
deserializeWithStrings iterationCount = S.runSouffle Benchmarks $ \case
  Nothing -> liftIO $ print "Failed to load deserialize benchmarks!"
  Just prog -> do
    S.run prog
    replicateM_ iterationCount $ do
      fs <- S.getFacts prog
      pure (fs :: V.Vector FromDatalogStringFact)

serializationBenchmarks :: [Benchmark]
serializationBenchmarks =
  [ bgroup "serializing facts (without strings)"
    [ bench "1"      $ nfIO $ serializeNumbers 1
    , bench "10"     $ nfIO $ serializeNumbers 10
    , bench "100"    $ nfIO $ serializeNumbers 100
    , bench "1000"   $ nfIO $ serializeNumbers 1000
    , bench "10000"  $ nfIO $ serializeNumbers 10000
    ]
  , bgroup "serializing facts (with strings)"
    [ bench "1"      $ nfIO $ serializeWithStrings 1
    , bench "10"     $ nfIO $ serializeWithStrings 10
    , bench "100"    $ nfIO $ serializeWithStrings 100
    , bench "1000"   $ nfIO $ serializeWithStrings 1000
    , bench "10000"  $ nfIO $ serializeWithStrings 10000
    ]
  ]

deserializationBenchmarks :: [Benchmark]
deserializationBenchmarks =
  [ bgroup "deserializing facts (without strings)"
    [ bench "1"      $ nfIO $ deserializeNumbers 1
    , bench "10"     $ nfIO $ deserializeNumbers 10
    , bench "100"    $ nfIO $ deserializeNumbers 100
    , bench "1000"   $ nfIO $ deserializeNumbers 1000
    , bench "10000"  $ nfIO $ deserializeNumbers 10000
    ]
  , bgroup "deserializing facts (with strings)"
    [ bench "1"      $ nfIO $ deserializeWithStrings 1
    , bench "10"     $ nfIO $ deserializeWithStrings 10
    , bench "100"    $ nfIO $ deserializeWithStrings 100
    , bench "1000"   $ nfIO $ deserializeWithStrings 1000
    , bench "10000"  $ nfIO $ deserializeWithStrings 10000
    ]
  ]
