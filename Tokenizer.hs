module Tokenizer where

import Data.Char
import Data.List (elem)
import Tokenizer.Token

tokenize :: String -> [Token]
tokenize "" = []
tokenize expr@(c:cs)
  | isDigit c = 
      let (num, rest) = span isNumberChar expr
      in Value (read num) : tokenize rest
  | isLetter c =
      let (ident, rest) = span isLetter expr
      in Identifier ident : tokenize rest
  | isSpace c = tokenize cs
  | elem c operators = Operation (read [c]) : tokenize cs
  | elem c delimiters = Delimiter (read [c]) : tokenize cs
  | otherwise = error "Not implemented"

isNumberChar :: Char -> Bool
isNumberChar c = isDigit c || c == '.'
