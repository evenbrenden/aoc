module Common (get, put) where

import Text.Trifecta

get :: FilePath -> IO String
get = readFile

parseEntireString :: Parser a -> String -> Result a
parseEntireString parser input = parseString parser mempty input

printResult :: Show a => Result a -> IO ()
printResult result =
    case result of
        Success output -> print output
        Failure error' -> print error'

put :: Show a
    => Parser b
    -> (b -> a)
    -> String
    -> IO ()
put parser solver input =
    printResult $ solver <$> parseEntireString parser input
