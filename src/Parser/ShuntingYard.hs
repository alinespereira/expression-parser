module Parser.ShuntingYard (parse) where

-- import Data.Maybe (fromJust)
import Data.Sequence
-- import Tokenizer.Operator (Operator, precedence)
import Tokenizer.Token

type Stack = Seq Token

parse :: Seq Token -> Seq Token
parse tokens = parse' tokens Empty

parse' :: Seq Token -> Stack -> Seq Token
parse' Empty Empty = Empty
parse' Empty stack = foldl (|>) Empty stack
parse' (t :<| ts) stack
  | isValue t || isIdentifier t = t <| parse' ts stack
  | isOperation t = parseOp t ts stack
  | otherwise = undefined
-- parse' [] (op : ops) = (parse' [] ops) ++ [Operation op]
-- parse' (t@(Value _) : ts) ops = t : parse' ts ops
-- parse' (t@(Identifier _) : ts) ops = t : parse' ts ops
-- parse' ((Operation o) : ts) [] = parse' ts [o]
-- parse' (t@(Operation o) : ts) (op : ops)
--   | precedence o < precedence op = (parse' (t : ts) ops) ++ [Operation op]
--   | otherwise = (parse' ts (o : op : ops))
-- parse' ((Delimiter _) : _) _ = undefined

parseOp :: Token -> Seq Token -> Stack -> Stack
parseOp t@(Operation _) ts stack@Empty = parse' ts (t <| stack)
parseOp t@(Operation _) ts stack@(op :<| ops)
  | t < op = op <| parse' (t <| ts) ops
  | otherwise = parse' ts (t <| stack)
parseOp _ _ _ = error "Not implemented"