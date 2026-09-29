{-# LANGUAGE ViewPatterns #-}

module Lexer where

import Control.Applicative (Alternative, empty, many, (<|>))
import Data.Char (isSpace)

-- regex for variable names and the like, same as C
-- [a-zA-Z_][a-zA-Z_0-9]*

data SingleCharTokenType = LeftParen | RightParen | LeftBrace | RightBrace | Comma | Dot | Minus | Plus | Semicolon | Slash | Star deriving (Show, Eq)

data FewCharTokenType = Bang | BangEqual | Equal | EqualEqual | Greater | GreaterEqual | Lesser | LesserEqual deriving (Show, Eq)

data LiteralTokenType = Identifier String | String' String | Number Double deriving (Show, Eq)

data KeywordsTokenType = And | Class | Else | False' | True' | Fun | For | If | Nil | Or | Print | Return | Super | This | Var | While deriving (Show, Eq)

data TokenType
  = SingleChar SingleCharTokenType
  | FewChar FewCharTokenType
  | Literal LiteralTokenType
  | Keyword KeywordsTokenType
  | Eof
  deriving (Show, Eq)

type LineNumber = Int

type ColumnNumber = Int

-- TODO: populate lexeme/line/column once in a
-- `located :: Parser TokenType -> Parser Token` combinator rather than in every
-- token rule, since the rules below all use `<$` and discard the text they
-- matched. Recovering the lexeme via `length (inputStr i) - length (inputStr
-- i')` is tempting but O(n) per token since length walks the remaining input,
-- so O(n*m) overall; restore an absolute offset field on Input (like the old
-- inputLoc) so `located` can slice with `take (off' - off) (inputStr i)`
-- instead, which is O(token length). Cheaper to decide this now than after
-- located is written and every token position needs re-checking.
data Token = Token {tokenType :: TokenType, lexeme :: String, lineNumber :: LineNumber, columnNumber :: ColumnNumber} deriving (Show, Eq)

data ParserError = Unexpected LineNumber ColumnNumber String | UnexpectedEof LineNumber String deriving (Show, Eq)

data Input = Input
  { inputLine :: LineNumber,
    inputCol :: ColumnNumber,
    inputStr :: String
  }
  deriving (Show, Eq)

-- TODO: lines are 1 indexed like the book, but columns start at 0 here and
-- inputUncons resets to 0 on '\n' (both consistent, so no drift), which means
-- every error currently reads "line 1, column 0" for the first character.
-- Decide 0- vs 1-indexed columns before writing `located`, since it stamps
-- inputCol onto every Token; 1-indexed to match lines is the natural choice.
-- lines will be 1 indexed like the book
mkInputAt :: LineNumber -> String -> Input
mkInputAt line i = Input line 0 i

-- lines will be 1 indexed like the book
mkInput :: String -> Input
mkInput i = Input 1 0 i

inputUncons :: Input -> Maybe (Char, Input)
inputUncons (Input _ _ []) = Nothing
inputUncons (Input line _ ('\n' : xs)) = Just ('\n', Input (line + 1) 0 xs)
inputUncons (Input line col (x : xs)) = Just (x, Input line (col + 1) xs)

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

-- TODO: this discards the first error, so every message comes from the last
-- alternative tried: input '@' reports "Expected while, but found '@'". Keep
-- whichever error reached the furthest position instead. On a tie, prefer the
-- end-of-input failure: "might still be completable" tells the REPL more than
-- "definitely wrong here".
instance Alternative (Either ParserError) where
  -- unreachable in practice (many/optional don't route through empty), but
  -- line 0 and col 1 are inconsistent with each other regardless of which way
  -- the column-indexing TODO on mkInput is decided -- worth fixing alongside
  -- it if this ever becomes reachable
  empty = Left $ Unexpected 0 1 "empty"
  Left _ <|> e2 = e2
  e1 <|> _ = e1

charParser :: Char -> Parser Char
charParser x = Parser f
  where
    f input@(inputUncons -> Just (y, ys))
      | y == x = Right (ys, x)
      | otherwise =
          Left $
            Unexpected
              (inputLine input)
              (inputCol input)
              ("Expected '" ++ [x] ++ "', but found '" ++ [y] ++ "'")
    f input =
      Left $
        UnexpectedEof
          (inputLine input)
          ("Expected '" ++ [x] ++ "', but reached end of string")

stringParser :: String -> Parser String
stringParser str = Parser f
  where
    f input = case runParser (traverse charParser str) input of
      Left _ ->
        Left $ Unexpected (inputLine input) (inputCol input) ("Expected \"" ++ str ++ "', but found '" ++ inputStr input ++ "'")
      result -> result

-- parse strings that statisfy a predicate
-- TODO: this succeeds on zero characters. Anything built on it that feeds an
-- outer `many` must be forced to consume at least one char, or the outer `many`
-- spins forever.
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
              Unexpected (inputLine input) (inputCol input) ("Expected " ++ desc ++ ", but found '" ++ [y] ++ "'")
      _ -> Left $ UnexpectedEof (inputLine input) ("Expected " ++ desc ++ ", but reached the end of a string")

-- white space parser
ws :: Parser String
ws = spanParser "whitespace character" isSpace

-- parser of character that is not " or \\
normalChar :: Parser Char
normalChar = parseIf "non-special character" ((&&) <$> (/= '"') <*> (/= '\\'))

-- parser of string between double quotes
stringLiteral :: Parser String
stringLiteral = charParser '"' *> many normalChar <* charParser '"'

-- TODO: this parser goes away entirely once keywords are recognised via the
-- identifier lookup below.
boolToken :: Parser TokenType
boolToken = tokenTrue <|> tokenFalse
  where
    tokenTrue = Keyword True' <$ stringParser "true"
    tokenFalse = Keyword False' <$ stringParser "false"

-- TODO: no test coverage, unlike fewCharToken and keywordToken
singleCharToken :: Parser TokenType
singleCharToken = leftParen <|> rightParen <|> leftBrace <|> rightBrace <|> comma <|> dot <|> minus <|> plus <|> semicolon <|> slash <|> star
  where
    leftParen = SingleChar LeftParen <$ stringParser "("
    rightParen = SingleChar RightParen <$ stringParser ")"
    leftBrace = SingleChar LeftBrace <$ stringParser "{"
    rightBrace = SingleChar RightBrace <$ stringParser "}"
    comma = SingleChar Comma <$ stringParser ","
    dot = SingleChar Dot <$ stringParser "."
    minus = SingleChar Minus <$ stringParser "-"
    plus = SingleChar Plus <$ stringParser "+"
    semicolon = SingleChar Semicolon <$ stringParser ";"
    -- TODO: when line comments arrive, the "//" parser has to be tried before
    -- this one, and singleCharToken is the first alternative in tokenizer
    slash = SingleChar Slash <$ stringParser "/"
    star = SingleChar Star <$ stringParser "*"

-- Ordering is load-bearing: <|> retries from the original input, so the longest
-- alternative must come first or "!=" stops after matching the '!'.
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
    -- TODO: must consume at least one character, so build it as
    -- (:) <$> parseIf alphaOrUnderscore <*> spanParser alphaNumOrUnderscore.
    -- A bare spanParser succeeds on the empty match, which makes tokenizer
    -- succeed without advancing and hangs the outer `many` on input like '@'.
    -- TODO: this is also where keyword recognition belongs -- look the matched
    -- text up in a keyword table and fall back to Literal Identifier.
    identifier = undefined
    number = undefined
    -- TODO: wrap stringLiteral
    string = undefined

-- TODO: a "//" comment parser has to precede singleCharToken here, otherwise
-- slash claims the first '/'.
-- TODO: the book reports "Unexpected character." and keeps scanning, so one run
-- surfaces every bad character; failing on Either stops at the first. To match
-- it, add a last alternative that always succeeds by consuming one char and
-- recording an error. Note that makes the outer `many` unable to fail, which
-- demotes the trailing `eof` below from the thing that catches garbage to a
-- redundant safety net.
tokenizer :: Parser TokenType
tokenizer = singleCharToken <|> fewCharToken <|> keywordToken <|> literalToken

-- TODO: both of these sketches are dead ends, replace them with a single
-- `many (ws *> located tokenizer) <* ws <* eof`. The repetition that jsonValue
-- got from being a recursive data type comes from `many` instead -- TokenType
-- does not need to recurse, and there is no need to thread Right results by
-- hand.
--
-- `words` cannot be used: Lox tokens are not whitespace delimited, e.g. "a+b"
-- and "(1+2);". `lines` cannot be used either, because Lox strings may span
-- newlines -- see the Input TODO above.
--
-- Two traps in `many` to guard against:
--   1. it stops silently on the first failure, so "var x = @@@" would report
--      three tokens and success. An `eof :: Parser ()` that fails on leftover
--      input is what turns that into an error.
--   2. it loops forever if tokenizer can succeed without consuming input --
--      see the identifier TODO.
--
-- This shape is unchanged by the REPL. Input is just a String, so the lexer
-- does not care where the text came from; the driver decides how much text is
-- one lex, and each REPL entry is a complete source that must be fully
-- consumed, so the trailing `eof` is still wanted there.

-- tokenizeLine :: String -> [TokenType]
-- tokenizeLine line = (runParser tokenizer) <$> (words line)

-- scanTokens :: String -> [Token]
-- scanTokens str = zipWith mapLine [0..] $ lines str
--     where mapLine lineNum line = do
