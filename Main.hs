module Main where

import Tokenizer (tokenize)

main = do
  putStrLn $ show $ tokenize "x + 2 * (x ^ 2)"