module Main where

import Control.Monad
import System.IO
import Text.Trifecta

parseInt :: Parser Int
parseInt = fromIntegral <$> integer

parseInts :: Parser [Int]
parseInts = do
    is <- many parseInt
    eof
    return is

parseInput :: String -> [Int]
parseInput = handleResult . parseString parseInts mempty

handleResult (Success is) = is
handleResult (Failure f) = error $ show f

part1 :: [Int] -> Int
part1 ints = head $ do
    x <- ints
    y <- ints
    guard $ x + y == 2020
    return $ x * y

part2 :: [Int] -> Int
part2 ints = head $ do
    x <- ints
    y <- ints
    z <- ints
    guard $ x + y + z == 2020
    return $ x * y * z

main :: IO ()
main = do
    handle <- openFile "app/Day1Of2020.txt" ReadMode
    contents <- hGetContents handle
    print $ part1 . parseInput $ contents
    print $ part2 . parseInput $ contents
