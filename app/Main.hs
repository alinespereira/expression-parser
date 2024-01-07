module Main where

import Parser.ShuntingYard (parse)
import Tokenizer (tokenize)

main = do
  let tokens = tokenize "1 + 2 * 5"
  print tokens
  -- print $ parse $ take 3 tokens