module Lexer where

-- regex for variable names and the like, same as C
-- [a-zA-Z_][a-zA-Z_0-9]*

data SingleCharTokenType = LeftParen | RightParen | LeftBrace | RightBrace | Comma | Dot | Minus | Plus | Semicolon | Slash | Star

data FewCharTokenType = Bang | BangEqual | Equal | EqualEqual | Greater | GreaterEqual | Lesser | LesserEqual

data LiteralTokeType = Identifer | String | Number

data Keywords = And | Class | Else | False' | True' | Fun | For | If | Nil | Or | Print | Return | Super | This | Var | While

data TokenType = SingleCharTokenType | FewCharTokenType | LiteralTokeType | Keywords | Eof

data Token = Token {tokenType :: TokenType, lexeme :: String, line :: Int}

type NumberedLine = (Int, String)

type NumberedLines = [NumberedLine]

scanTokens :: String -> [Token]
