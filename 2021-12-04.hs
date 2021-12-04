#! /usr/bin/env nix-shell
#! nix-shell -i "ghcid -c 'ghci -Wall -Wno-unused-do-bind' -T main"

module Main where

import Common
import Text.Trifecta

type Calls  = [Int]
type Row    = [Int]
type Board  = [Row]

parseCalls :: Parser Calls
parseCalls = do
    calls <- integer `sepBy` (char ',')
    return $ fromInteger <$> calls

parseRow :: Parser Row
parseRow = do
    a <- integer
    b <- integer
    c <- integer
    d <- integer
    e <- integer
    return $ fromIntegral <$> [a, b, c, d, e]

parseBoard :: Parser Board
parseBoard = do
    a <- parseRow
    b <- parseRow
    c <- parseRow
    d <- parseRow
    e <- parseRow
    return [a, b, c, d, e]

parseInput :: Parser (Calls, [Board])
parseInput = do
    calls <- parseCalls
    boards <- many parseBoard
    eof
    return (calls, boards)

main :: IO ()
main = do
    input <- get "2021-12-04.txt"
    put parseInput id input
