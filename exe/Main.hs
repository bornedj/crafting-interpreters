module Main (main) where

import System.IO (hFlush, stdout)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  parseArgs args

parseArgs :: [String] -> IO ()
parseArgs [] = runPrompt
parseArgs [arg] = runFile arg
parseArgs _ = error "Usage: jlox [script]"

-- TODO: getLine throws on end of input, so piping a script in
-- (`echo 'var x = 1;' | jlox`) dies with "<stdin>: hGetLine: end of file" and
-- exit 1 instead of exiting cleanly. Guard with System.IO.isEOF.
-- TODO: an empty line quits, which means enter on a bare prompt ends the
-- session. Only end of input should exit.
-- TODO: this loop is where a line counter would be threaded, if entries are to
-- be numbered cumulatively rather than each restarting at line 1 -- see the
-- Input TODO in Lexer for the decision.
runPrompt :: IO ()
runPrompt = do
    putStr "> "
    hFlush stdout
    line <- getLine
    case line of
        "" -> pure ()
        _ -> do
            run line
            runPrompt

runFile :: String -> IO ()
runFile path = do
    contents <- readFile path
    run contents

run :: String -> IO ()
run source = do
    putStrLn source
