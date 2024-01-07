module Tokenizer.Token where

data Token = Value Float
           | Operation Operator
           | Delimiter Delim
           | Identifier String
  deriving (Show, Eq)

data Operator = Plus
              | Minus
              | Times
              | Divide
              | Power
  deriving (Show, Eq)

instance Read Operator where
  readsPrec _ (c:rest) =
    case c of
      '+' -> [(Plus, rest)]
      '-' -> [(Minus, rest)]
      '*' -> [(Times, rest)]
      '/' -> [(Divide, rest)]
      '^' -> [(Power, rest)]
      _ -> error "Not a valid operator"

operators :: [Char]
operators = ['+', '-', '*', '/', '^']

data Delim = LeftParen
           | RightParen
           | LeftBracket
           | RightBracket
           | LeftBrace
           | RightBrace
  deriving (Show, Eq)

instance Read Delim where
  readsPrec _ (d:rest) =
    case d of
      '(' -> [(LeftParen, rest)]
      ')' -> [(RightParen, rest)]
      '[' -> [(LeftBracket, rest)]
      ']' -> [(RightBracket, rest)]
      '{' -> [(LeftBrace, rest)]
      '}' -> [(RightBrace, rest)]
      _ -> error "Not a valid delimiter"

delimiters :: [Char]
delimiters = ['(', ')', '[', ']', '{', '}']