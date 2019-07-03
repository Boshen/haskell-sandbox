module BrainfuckSpec(spec) where

import           Foundation

import           Test.Hspec
import           Text.Megaparsec

import           Brainfuck

spec :: Spec
spec =
  describe "Brainfuck" $ do
    describe "Parser" $
      it "should parse" $ do
        parseMaybe parser "><+-.," `shouldBe` Just [MoveRight, MoveLeft, Increment, Decrement, Print, Read]
        parseMaybe parser "[-]" `shouldBe` Just [Loop [Decrement]]
        parseMaybe parser "test" `shouldBe` Just [Comment 't', Comment 'e', Comment 's', Comment 't']
        parseMaybe parser "+." `shouldBe` Just [Increment, Print]

    describe "Interpreter" $ do
      it "should interpret" $ do
        interpret emptyTape "+." `shouldBe` [1]
        interpret emptyTape "-." `shouldBe` [-1]

      it "should loop" $ do
        interpret emptyTape "+++[-.]" `shouldBe` [2, 1, 0]
        interpret (Tape [] 1 [1]) "[->+<]>." `shouldBe` [2]
