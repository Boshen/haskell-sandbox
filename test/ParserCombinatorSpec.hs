module ParserCombinatorSpec(spec) where

import qualified Data.Char        as C
import           Data.Text        (Text)
import           Foundation
import           Test.Hspec

import           ParserCombinator

spec :: Spec
spec =
  describe "ParserCombinator" $ do
    it "item" $ do
      parse item "" `shouldBe` []
      parse item "f" `shouldBe` [('f', "")]
      parse item "foo" `shouldBe` [('f', "oo")]

    it "MonadZero" $ do
      parse zero "" `shouldBe` ([] :: [(Char, Text)])
      parse zero "foo" `shouldBe` ([] :: [(Char, Text)])

    it "MonadPlus" $
      parse (zero ++ zero) "foo" `shouldBe` ([] :: [(Char, Text)])

    it "Functor" $
      parse (C.toUpper <$> item) "foo" `shouldBe` [('F', "oo")]

    it "satisfy" $ do
      parse (satisfy (const True)) "foo" `shouldBe` [('f', "oo")]
      parse (satisfy (const False)) "foo" `shouldBe` []

    it "char" $ do
      parse (char 'f') "foo" `shouldBe` [('f', "oo")]
      parse (char 'x') "foo" `shouldBe` []

    it "string" $ do
      parse (string "foo") "f" `shouldBe` []
      parse (string "fo") "foo" `shouldBe` [("fo", "o")]
      parse (string "foo") "foo" `shouldBe` [("foo", "")]

    it "many" $ do
      parse (many $ char 'f') "" `shouldBe` [("", "")]
      parse (many $ char 'f') "ffxx" `shouldBe` [("ff", "xx")]
      parse (many $ char 'f') "ffff" `shouldBe` [("ffff", "")]

    it "many1" $ do
      parse (many1 $ char 'f') "" `shouldBe` []
      parse (many1 $ char 'f') "ffxx" `shouldBe` [("ff", "xx")]
      parse (many1 $ char 'f') "ffff" `shouldBe` [("ffff", "")]

    it "sepby" $ do
      parse (sepBy (char 'f') (char ';')) "f;f;f" `shouldBe` [("", "")]
      parse (sepBy (char 'f') (char ';')) ";f;f" `shouldBe` []
      parse (sepBy (char 'f') (char ';')) "f;" `shouldBe` [("", ";")]

    it "sepby1" $ do
      parse (sepBy1 (char 'f') (char ';')) "f;f;f" `shouldBe` [("fff", "")]
      parse (sepBy1 (char 'f') (char ';')) ";f;f" `shouldBe` []
      parse (sepBy1 (char 'f') (char ';')) "f;" `shouldBe` [("f", ";")]
