module Parser.ShuntingYardSpec where

import Data.Sequence (fromList)
import Parser.ShuntingYard (parse)
import SpecHelper
import Tokenizer (tokenize)
import Tokenizer.Operator
import Tokenizer.Token (Token (..))

spec :: Spec
spec =
  describe "Parser" $ do
    context "only tokens" $ do
      it "tokenizes a series of numbers" $ do
        (parse . tokenize) "1 2 3 4"
          `shouldBe` (fromList . map Value) [1 .. 4]
    context "simple expression" $ do
      it "tokenizes a sum" $ do
        (parse . tokenize) "1 + 2"
          `shouldBe` fromList [Value 1, Value 2, Operation Plus]
      it "tokenizes a subtraction" $ do
        (parse . tokenize) "1 - 2"
          `shouldBe` fromList [Value 1, Value 2, Operation Minus]
      it "tokenizes a multiplication" $ do
        (parse . tokenize) "1 * 2"
          `shouldBe` fromList [Value 1, Value 2, Operation Times]
      it "tokenizes a division" $ do
        (parse . tokenize) "1 / 2"
          `shouldBe` fromList [Value 1, Value 2, Operation Divide]
      it "tokenizes power" $ do
        (parse . tokenize) "1 ^ 2"
          `shouldBe` fromList [Value 1, Value 2, Operation Power]

      it "ignores whitespaces" $ do
        (parse . tokenize) "     1  +   2   "
          `shouldBe` fromList [Value 1, Value 2, Operation Plus]

    context "precedence handling" $ do
      it "tokenizes a simple expression with precedence" $ do
        (parse . tokenize) "1 + 2 * 5"
          `shouldBe` fromList
            [ Value 1,
              Value 2,
              Value 5,
              Operation Times,
              Operation Plus
            ]

      it "parses a complex expression" $ do
        (parse . tokenize) "3 + 4 * 2 / 4 ^ 2 ^ 3"
          `shouldBe` fromList
            [ Value 3,
              Value 4,
              Value 2,
              Operation Times,
              Value 4,
              Value 2,
              Operation Power,
              Value 3,
              Operation Power,
              Operation Divide,
              Operation Plus
            ]

main :: IO ()
main = hspec spec