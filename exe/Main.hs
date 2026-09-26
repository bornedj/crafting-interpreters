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
