module Tokenizer where

import Data.Char
import Data.Sequence
import Tokenizer.Delimiter (delimiters)
import Tokenizer.Operator (operators)
import Tokenizer.Token

tokenize :: String -> Seq Token
tokenize "" = Empty
tokenize expr@(c : cs)
  | isDigit c =
      let (num, rest) = span isNumberChar expr
       in Value (read num) <| tokenize rest
  | isLetter c =
      let (ident, rest) = span isLetter expr
       in Identifier ident <| tokenize rest
  | isSpace c = tokenize cs
  | c `elem` operators = Operation (read [c]) <| tokenize cs
  | c `elem` delimiters = Delimiter (read [c]) <| tokenize cs
  | otherwise = error "Not implemented"

isNumberChar :: Char -> Bool
isNumberChar c = isDigit c || c == '.'
