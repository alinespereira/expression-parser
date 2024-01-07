module TokenizerSpec where

import Data.Sequence (fromList)
import SpecHelper
import Tokenizer
import Tokenizer.Operator
import Tokenizer.Token

spec :: Spec
spec =
  describe "Tokenizer" $ do
    context "only tokens" $ do
      it "tokenizes a series of numbers" $ do
        tokenize "1 2 3 4"
          `shouldBe` (fromList . map Value) [1 .. 4]
    context "simple expression" $ do
      it "tokenizes a sum" $ do
        tokenize "1 + 2"
          `shouldBe` fromList [Value 1, Operation Plus, Value 2]
      it "tokenizes a subtraction" $ do
        tokenize "1 - 2"
          `shouldBe` fromList [Value 1, Operation Minus, Value 2]
      it "tokenizes a multiplication" $ do
        tokenize "1 * 2"
          `shouldBe` fromList [Value 1, Operation Times, Value 2]
      it "tokenizes a division" $ do
        tokenize "1 / 2"
          `shouldBe` fromList [Value 1, Operation Divide, Value 2]
      it "tokenizes power" $ do
        tokenize "1 ^ 2"
          `shouldBe` fromList [Value 1, Operation Power, Value 2]

      it "ignores whitespaces" $ do
        tokenize "     1  +   2   "
          `shouldBe` fromList [Value 1, Operation Plus, Value 2]

    context "precedence handling" $ do
      it "tokenizes a simple expression with precedence" $ do
        tokenize "1 + 2 * 5"
          `shouldBe` fromList
            [ Value 1,
              Operation Plus,
              Value 2,
              Operation Times,
              Value 5
            ]

      it "parses a complex expression" $ do
        tokenize "3 + 4 * 2 / 4 ^ 2 ^ 3"
          `shouldBe` fromList
            [ Value 3,
              Operation Plus,
              Value 4,
              Operation Times,
              Value 2,
              Operation Divide,
              Value 4,
              Operation Power,
              Value 2,
              Operation Power,
              Value 3
            ]

main :: IO ()
main = hspec spec