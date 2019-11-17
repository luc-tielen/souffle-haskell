
module Test.TestSuite ( module Test.TestSuite ) where

import Test.Hspec


spec_suite :: Spec
spec_suite = describe "Test suite" $ parallel $ do
  it "can run a single test" $ 1 + 1 `shouldBe` 2

  it "can run multiple tests" $ do
    1 + 1 `shouldBe` 2
    1 + 2 `shouldBe` 3
