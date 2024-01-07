module Parser where

import Tokenizer.Operator
import Tokenizer.Token

data Expr
  = Const Float
  | Expr Operator Expr Expr
  deriving (Eq, Show)

parse :: [Token] -> Expr
parse = undefined