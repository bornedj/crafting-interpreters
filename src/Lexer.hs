{-# LANGUAGE ViewPatterns #-}

module Lexer where

import Control.Applicative (Alternative, empty, (<|>))

-- regex for variable names and the like, same as C
-- [a-zA-Z_][a-zA-Z_0-9]*

data SingleCharTokenType = LeftParen | RightParen | LeftBrace | RightBrace | Comma | Dot | Minus | Plus | Semicolon | Slash | Star deriving (Show, Eq)

data FewCharTokenType = Bang | BangEqual | Equal | EqualEqual | Greater | GreaterEqual | Lesser | LesserEqual deriving (Show, Eq)

data LiteralTokeType = Identifer | String | Number deriving (Show, Eq)

data KeywordsTokenType = And | Class | Else | False' | True' | Fun | For | If | Nil | Or | Print | Return | Super | This | Var | While deriving (Show, Eq)

data TokenType = SingleChar SingleCharTokenType
                  | FewChar FewCharTokenType
                  | Literal LiteralTokeType
                  | Keyword KeywordsTokenType
                  deriving (Show, Eq)

data Token = Token {tokenType :: TokenType, lexeme :: String, line :: Int}

data ParserError = ParserError Int String deriving (Show)

data Input = Input
  { inputLoc :: Int,
    inputStr :: String
  }
  deriving (Show, Eq)

inputUncons :: Input -> Maybe (Char, Input)
inputUncons (Input _ []) = Nothing
inputUncons (Input loc (x : xs)) = Just (x, Input (loc + 1) xs)

newtype Parser a = Parser {runParser :: Input -> Either ParserError (Input, a)}

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      return (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      return (input'', f a)

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

instance Alternative (Either ParserError) where
  empty = Left $ ParserError 0 "empty"
  Left _ <|> e2 = e2
  e1 <|> _ = e1

charParser :: Char -> Parser Char
charParser x = Parser f
  where
    f input@(inputUncons -> Just (y, ys))
      | y == x = Right (ys, x)
      | otherwise =
          Left $
            ParserError
              (inputLoc input)
              ("Expected '" ++ [x] ++ "', but found '" ++ [y] ++ "'")
    f input =
      Left $
        ParserError
          (inputLoc input)
          ("Expected '" ++ [x] ++ "', but reached end of string")

stringParser :: String -> Parser String
stringParser str = Parser f
  where
    f input = case runParser (traverse charParser str) input of
      Left _ ->
        Left $ ParserError (inputLoc input) ("Expected \"" ++ str ++ "', but found '" ++ inputStr input ++ "'")
      result -> result

boolToken :: Parser TokenType
boolToken = tokenTrue <|> tokenFalse
    where
        tokenTrue = Keyword True' <$ stringParser "True"
        tokenFalse = Keyword False' <$ stringParser "False"
