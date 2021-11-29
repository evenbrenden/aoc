module Main where

import Common
import Control.Monad
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
    input <- load "app/2020day1.txt"
    print $ part1 . parseInput $ input
    print $ part2 . parseInput $ input
