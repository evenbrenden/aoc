-- https://adventofcode.com/2020/day/1

module Main where

import System.IO
import Text.Trifecta

int :: Parser Int
int = fromIntegral <$> integer

allInts :: Parser [Int]
allInts = do
    is <- many int
    eof
    return is

parseAllInts :: String -> Result [Int]
parseAllInts = parseString allInts mempty

handleResult (Success is) = is
handleResult (Failure f) = error $ show f

parse :: String -> [Int]
parse = handleResult . parseAllInts

solve :: [Int] -> [Int]
solve = id

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print $ solve . parse $ contents
