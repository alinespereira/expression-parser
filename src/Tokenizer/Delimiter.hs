module Tokenizer.Delimiter where

data Delimiter
  = LeftParen
  | RightParen
  | LeftBracket
  | RightBracket
  | LeftBrace
  | RightBrace
  deriving (Show, Eq)

instance Read Delimiter where
  readsPrec _ [] = []
  readsPrec _ (d : rest) =
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

isOpenDelimiter :: Delimiter -> Bool
isOpenDelimiter = (`elem` [LeftParen, LeftBracket, LeftBrace])

isCloseDelimiter :: Delimiter -> Bool
isCloseDelimiter = (`elem` [RightParen, RightBracket, RightBrace])

getMatchingDelimiter :: Delimiter -> Delimiter
getMatchingDelimiter d =
  case d of
    LeftParen -> RightParen
    LeftBracket -> RightBracket
    LeftBrace -> RightBrace
    RightParen -> LeftParen
    RightBracket -> LeftBracket
    RightBrace -> LeftBrace

isMatchingDelimiter :: Delimiter -> Delimiter -> Bool
isMatchingDelimiter d1 d2 =
  getMatchingDelimiter d1 == d2
infixr 9 `isMatchingDelimiter`