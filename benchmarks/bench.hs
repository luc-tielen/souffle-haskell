{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main ( main ) where

import Criterion.Main
import qualified Language.Souffle.Compiled as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics
import Data.Word
import Data.Int
import Control.Monad.IO.Class
import Control.DeepSeq


data Benchmarks = Benchmarks

data NumbersFact
  = NumbersFact Word32 Int32 Float
  deriving (Generic, NFData, Show)

data StringsFact
  = StringsFact Word32 T.Text Int32 Float
  deriving (Generic, NFData)

instance S.Program Benchmarks where
  type ProgramFacts Benchmarks = '[NumbersFact, StringsFact]
  programName = const "bench"

instance S.Fact NumbersFact where
  type FactDirection NumbersFact = 'S.InputOutput
  factName = const "numbers_fact"

instance S.Fact StringsFact where
  type FactDirection StringsFact = 'S.InputOutput
  factName = const "strings_fact"

instance S.Marshal NumbersFact
instance S.Marshal StringsFact


roundTrip :: (S.ContainsInputFact Benchmarks a, S.ContainsOutputFact Benchmarks a, S.Fact a)
          => V.Vector a -> IO (V.Vector a)
roundTrip vec = S.runSouffle Benchmarks $ \case
  Nothing -> do
    liftIO $ print "Failed to load benchmarks!"
    pure V.empty
  Just prog -> do
    S.addFacts prog vec
    -- No run needed
    S.getFacts prog


-- TODO: uncomment cases with larger numbers (crashes due to large memory allocations?)
main :: IO ()
main = defaultMain
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
    --, bench "1000"   $ nfIO $ roundTrip $ mkVecStr 1000
    --, bench "10000"  $ nfIO $ roundTrip $ mkVecStr 10000
    --, bench "100000" $ nfIO $ roundTrip $ mkVecStr 100000
    ]
  , bgroup "round trip facts (with long strings)"
    [ bench "1"      $ nfIO $ roundTrip $ mkVecLongStr 1
    , bench "10"     $ nfIO $ roundTrip $ mkVecLongStr 10
    , bench "100"    $ nfIO $ roundTrip $ mkVecLongStr 100
    --, bench "1000"   $ nfIO $ roundTrip $ mkVecLongStr 1000
    --, bench "10000"  $ nfIO $ roundTrip $ mkVecLongStr 10000
    --, bench "100000" $ nfIO $ roundTrip $ mkVecLongStr 100000
    ]
  ]
  where mkVec count = V.generate count $ \i -> NumbersFact (fromIntegral i) (-42) 3.14
        mkVecStr count = V.generate count $ \i -> StringsFact (fromIntegral i) "abcdef" (-42) 3.14
        mkVecLongStr count = V.generate count $ \i -> StringsFact (fromIntegral i) (T.replicate 10 "abcdef") (-42) 3.14
