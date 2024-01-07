module Tokenizer.Operator where

data Operator
  = Plus
  | Minus
  | Times
  | Divide
  | Power
  deriving (Show, Eq)

instance Read Operator where
  readsPrec _ [] = []
  readsPrec _ (c : rest) =
    case c of
      '+' -> [(Plus, rest)]
      '-' -> [(Minus, rest)]
      '*' -> [(Times, rest)]
      '/' -> [(Divide, rest)]
      '^' -> [(Power, rest)]
      _ -> error "Not a valid operator"

instance Ord Operator where
  compare op1 op2 = compare (precedence op1) (precedence op2)

operators :: [Char]
operators = ['+', '-', '*', '/', '^']

precedence :: Operator -> Int
precedence op = 
  case op of 
    Plus -> 2
    Minus -> 2
    Times -> 3
    Divide -> 3
    Power -> 4
