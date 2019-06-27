module ArrowSpec(spec) where

import           Control.Arrow
import           Control.Monad.Identity
import           Foundation             hiding (first, second)
import           Test.Hspec

import           Arrow

spec :: Spec
spec =
  describe "Arrow" $
    it "should test" $ do
      f 2 `shouldBe` (7, -1)
      runIdentity (fM 2) `shouldBe` (7, -1)
      fA 2 `shouldBe` (7, 1)
      (f1 *** f2) (2, 2) `shouldBe` (4, 5)
      (f1 &&& f2) 4 `shouldBe` (8, 7)
      (f1 >>> f2) 4 `shouldBe` 11
      (f1 <<< f2) 4 `shouldBe` 14
      first f1 (4, 4) `shouldBe` (8 :: Int, 4 :: Int)
      second f1 (4, 4) `shouldBe` (4 :: Int, 8 :: Int)
