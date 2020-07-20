
{-# LANGUAGE DeriveGeneric #-}

module Test.Language.Souffle.MarshalSpec
  ( module Test.Language.Souffle.MarshalSpec
  ) where

import Test.Hspec
import GHC.Generics
import Language.Souffle.Marshal
import Data.Text
import Data.Int
import Data.Word


data Edge = Edge String String
  deriving (Eq, Show, Generic)

newtype EdgeUInt = EdgeUInt Word32
  deriving (Eq, Show, Generic)

newtype FloatValue = FloatValue Float
  deriving (Eq, Show, Generic)

data EdgeStrict = EdgeStrict !String !String
  deriving (Eq, Show, Generic)

data EdgeUnpacked
  = EdgeUnpacked {-# UNPACK #-} !Int32 {-# UNPACK #-} !Int32
  deriving (Eq, Show, Generic)

type Vertex = Text
type Vertex' = Text

data EdgeSynonyms = EdgeSynonyms Vertex Vertex
  deriving (Eq, Show, Generic)

data EdgeMultipleSynonyms = EdgeMultipleSynonyms Vertex Vertex'
  deriving (Eq, Show, Generic)

data EdgeMixed = EdgeMixed Text Vertex
  deriving (Eq, Show, Generic)

data EdgeRecord
  = EdgeRecord
  { fromNode :: Text
  , toNode :: Text
  } deriving (Eq, Show, Generic)

data IntsAndStrings = IntsAndStrings Text Int32 Text
  deriving (Eq, Show, Generic)

data LargeRecord
  = LargeRecord Int32 Int32 Int32 Int32
  deriving (Eq, Show, Generic)


instance Marshal Edge
instance Marshal EdgeUInt
instance Marshal FloatValue
instance Marshal EdgeStrict
instance Marshal EdgeUnpacked
instance Marshal EdgeSynonyms
instance Marshal EdgeMultipleSynonyms
instance Marshal EdgeMixed
instance Marshal EdgeRecord
instance Marshal IntsAndStrings
instance Marshal LargeRecord


spec :: Spec
spec = describe "Auto-deriving marshalling code" $
  it "can generate code for all instances in this file" $
    -- If this file compiles, then the test has already passed
    42 `shouldBe` 42
