module Main where

import Parser.ShuntingYard (parse)
import Tokenizer (tokenize)

main :: IO ()
main = do
  let tokens = tokenize "1 + 2 * 5"
  print tokens
  print $ parse tokens