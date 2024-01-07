module Parser.ShuntingYard (parse) where

-- import Data.Maybe (fromJust)
import Tokenizer.Operator (Operator, precedence)
import Tokenizer.Token

parse :: [Token] -> [Token]
parse tokens = parse' (reverse tokens) []

parse' :: [Token] -> [Operator] -> [Token]
parse' [] [] = []
-- parse' [] (op : ops) = (parse' [] ops) ++ [Operation op]
-- parse' (t@(Value _) : ts) ops = t : parse' ts ops
-- parse' (t@(Identifier _) : ts) ops = t : parse' ts ops
-- parse' ((Operation o) : ts) [] = parse' ts [o]
-- parse' (t@(Operation o) : ts) (op : ops)
--   | precedence o < precedence op = (parse' (t : ts) ops) ++ [Operation op]
--   | otherwise = (parse' ts (o : op : ops))
-- parse' ((Delimiter _) : _) _ = undefined