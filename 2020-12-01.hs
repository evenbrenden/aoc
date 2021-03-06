#! /usr/bin/env nix-shell
#! nix-shell -i "ghcid -c 'ghci -Wall -Wno-unused-do-bind' -T main"

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

unsafePart1 :: [Int] -> Int
unsafePart1 ints = head $ do
    x <- ints
    y <- ints
    guard $ x + y == 2020
    return $ x * y

unsafePart2 :: [Int] -> Int
unsafePart2 ints = head $ do
    x <- ints
    y <- ints
    z <- ints
    guard $ x + y + z == 2020
    return $ x * y * z

main :: IO ()
main = do
    input <- get "2020-12-01.txt"
    put parseInts unsafePart1 input
    put parseInts unsafePart2 input
