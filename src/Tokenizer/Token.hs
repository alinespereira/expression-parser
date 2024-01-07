module Tokenizer.Token where

import Tokenizer.Delimiter (Delimiter)
import Tokenizer.Operator (Operator)

data Token
  = Value Float
  | Operation Operator
  | Delimiter Delimiter
  | Identifier String
  deriving (Show, Eq)

isValue :: Token -> Bool
isValue (Value _) = True
isValue _ = False

isOperation :: Token -> Bool
isOperation (Operation _) = True
isOperation _ = False

isDelimiter :: Token -> Bool
isDelimiter (Delimiter _) = True
isDelimiter _ = False

isIdentifier :: Token -> Bool
isIdentifier (Identifier _) = True
isIdentifier _ = False
