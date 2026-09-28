{-# LANGUAGE ViewPatterns #-}

module Lexer where

import Control.Applicative (Alternative, empty, many, (<|>))
import Data.Char (isSpace)

-- regex for variable names and the like, same as C
-- [a-zA-Z_][a-zA-Z_0-9]*

data SingleCharTokenType = LeftParen | RightParen | LeftBrace | RightBrace | Comma | Dot | Minus | Plus | Semicolon | Slash | Star deriving (Show, Eq)

data FewCharTokenType = Bang | BangEqual | Equal | EqualEqual | Greater | GreaterEqual | Lesser | LesserEqual deriving (Show, Eq)

data LiteralTokenType = Identifier | String' | Number deriving (Show, Eq)

data KeywordsTokenType = And | Class | Else | False' | True' | Fun | For | If | Nil | Or | Print | Return | Super | This | Var | While deriving (Show, Eq)

data TokenType
  = SingleChar SingleCharTokenType
  | FewChar FewCharTokenType
  | Literal LiteralTokenType
  | Keyword KeywordsTokenType
  deriving (Show, Eq)

data Token = Token {tokenType :: TokenType, lexeme :: String, lineNumber :: Int}

data ParserError = ParserError Int String deriving (Show, Eq)

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

-- parse strings that statisfy a predicate
spanParser :: String -> (Char -> Bool) -> Parser String
spanParser description = many . parseIf description

-- parser of charater that satisfies a predicate
parseIf :: String -> (Char -> Bool) -> Parser Char
parseIf desc predicate = Parser f
  where
    f input = case input of
      (inputUncons -> Just (y, ys))
        | predicate y -> Right (ys, y)
        | otherwise ->
            Left $
              ParserError (inputLoc input) ("Expected " ++ desc ++ ", but found '" ++ [y] ++ "'")
      _ -> Left $ ParserError (inputLoc input) ("Expected " ++ desc ++ ", but reached the end of a string")

-- white space parser
ws :: Parser String
ws = spanParser "whitespace character" isSpace

-- need to read the book to understand our escape chars
escapeChar :: Parser Char
escapeChar = undefined

-- parser of character that is not " or \\
normalChar :: Parser Char
normalChar = parseIf "non-special character" ((&&) <$> (/= '"') <*> (/= '\\'))

-- parser of string between double quotes
stringLiteral :: Parser String
stringLiteral = charParser '"' *> many (normalChar <|> escapeChar) <* charParser '"'

boolToken :: Parser TokenType
boolToken = tokenTrue <|> tokenFalse
  where
    tokenTrue = Keyword True' <$ stringParser "True"
    tokenFalse = Keyword False' <$ stringParser "False"

keywordToken :: Parser TokenType
keywordToken = and' <|> class' <|> else' <|> boolToken <|> fun <|> for' <|> if' <|> nil <|> or' <|> print' <|> return' <|> super <|> this <|> var <|> while
  where
    and' = Keyword And <$ stringParser "and"
    class' = Keyword Class <$ stringParser "class"
    else' = Keyword Else <$ stringParser "else"
    fun = Keyword Fun <$ stringParser "fun"
    for' = Keyword For <$ stringParser "for"
    if' = Keyword If <$ stringParser "if"
    nil = Keyword Nil <$ stringParser "nil"
    or' = Keyword Or <$ stringParser "or"
    print' = Keyword Print <$ stringParser "print"
    return' = Keyword Return <$ stringParser "return"
    super = Keyword Super <$ stringParser "super"
    this = Keyword This <$ stringParser "this"
    var = Keyword Var <$ stringParser "var"
    while = Keyword While <$ stringParser "while"

singleCharToken :: Parser TokenType
singleCharToken = leftParen <|> rightParen <|> leftBrace <|> rightBrace <|> comma <|> dot <|> minus <|> plus <|> semicolon <|> slash <|> star
  where
    leftParen = SingleChar LeftParen <$ stringParser "("
    rightParen = SingleChar RightParen <$ stringParser ")"
    leftBrace = SingleChar LeftBrace <$ stringParser "["
    rightBrace = SingleChar RightBrace <$ stringParser "]"
    comma = SingleChar RightBrace <$ stringParser ","
    dot = SingleChar Dot <$ stringParser "."
    minus = SingleChar Minus <$ stringParser "-"
    plus = SingleChar Plus <$ stringParser "+"
    semicolon = SingleChar Semicolon <$ stringParser ";"
    slash = SingleChar Slash <$ stringParser "/"
    star = SingleChar Star <$ stringParser "*"

fewCharToken :: Parser TokenType
fewCharToken = bangEqual <|> bang <|> equalEqual <|> equal <|> greaterEqual <|> greater <|> lesserEqual <|> lesser
  where
    bang = FewChar Bang <$ stringParser "!"
    bangEqual = FewChar BangEqual <$ stringParser "!="
    equal = FewChar Equal <$ stringParser "="
    equalEqual = FewChar EqualEqual <$ stringParser "=="
    greater = FewChar Greater <$ stringParser ">"
    greaterEqual = FewChar GreaterEqual <$ stringParser ">="
    lesser = FewChar Lesser <$ stringParser "<"
    lesserEqual = FewChar LesserEqual <$ stringParser "<="

literalToken :: Parser TokenType
literalToken = identifier <|> number <|> string
  where
    identifier = undefined
    number = undefined
    string = undefined

tokenizer :: Parser TokenType
tokenizer = singleCharToken <|> fewCharToken <|> keywordToken <|> literalToken

-- tokenizeLine :: String -> [TokenType]
-- tokenizeLine line = (runParser tokenizer) <$> (words line)

-- scanTokens :: String -> [Token]
-- scanTokens str = zipWith mapLine [0..] $ lines str
--     where mapLine lineNum line = do
