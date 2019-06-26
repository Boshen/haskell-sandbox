module LibSpec(spec) where

import           Foundation

import           Test.Hspec

import           Lib

spec :: Spec
spec =
  describe "Test" $
    it "should test" $
      foo `shouldBe` "Hello, Haskell!"
