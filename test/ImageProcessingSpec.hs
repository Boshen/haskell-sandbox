module ImageProcessingSpec(spec) where

import           Foundation
import           Test.Hspec

import           ImageProcessing

empty, grey, chartreuse, white, greyed :: Image
empty = Matrix 0 0 []
grey = Matrix 1 1 [[127], [127], [127]]
chartreuse = Matrix 1 1 [[127], [255], [0]]
white = Matrix 6 2 [[0,0,0,0,0,0],[0,0,0,255,255,255]]
greyed = Matrix 6 2 [[51,51,51,102,102,102],[102,102,102,204,204,204]]

nul, identity, blur, custom :: Kernel
nul = Matrix 0 0 []
identity = Matrix 1 1 [[1]]
blur = Matrix 3 3 [[1/9, 1/9, 1/9], [1/9, 1/9, 1/9], [1/9, 1/9, 1/9]]
custom = Matrix 3 3 [[0.2,0,0],[0,0.2,0.2],[0,0.2,0.2]]

spec :: Spec
spec =
  describe "Testing Image Processing" $ do
    it "empty" $
      processImage empty nul `shouldBe` empty

    it "identity kernel" $
      processImage grey identity `shouldBe` grey

    it "blur kernel" $
      processImage chartreuse blur `shouldBe` chartreuse

    it "custom kernel" $
      processImage white custom `shouldBe` greyed
