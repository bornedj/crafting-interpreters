module ErrorHandler where

type LineNum = Int

type Message = String

throwException :: LineNum -> Message -> IO ()
throwException ln message = report ln "" message

report :: LineNum -> String -> Message -> IO ()
report ln s m = putStrLn $ "[line " ++ (show ln) ++ "] Error" ++ s ++ ": " ++ m
