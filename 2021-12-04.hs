#! /usr/bin/env nix-shell
#! nix-shell -i "ghcid -c 'ghci -Wall -Wno-unused-do-bind' -T main"

module Main where

import Common
import Data.List
import Text.Trifecta

data Number = Number Int Bool deriving Show
type Board = [[Number]]

parseCalls :: Parser [Int]
parseCalls = do
    calls <- integer `sepBy` (char ',')
    return $ fromInteger <$> calls

parseRow :: Parser [Int]
parseRow = do
    a <- integer
    b <- integer
    c <- integer
    d <- integer
    e <- integer
    return $ fromIntegral <$> [a, b, c, d, e]

parseBoard :: Parser [[Int]]
parseBoard = do
    a <- parseRow
    b <- parseRow
    c <- parseRow
    d <- parseRow
    e <- parseRow
    return [a, b, c, d, e]

parseInput :: Parser ([Int], [[[Int]]])
parseInput = do
    calls <- parseCalls
    boards <- many parseBoard
    eof
    return (calls, boards)

initBoard :: [[Int]] -> Board
initBoard = fmap . fmap $ flip Number $ False

markNumber :: Int -> Number -> Number
markNumber x (Number n m) = Number n (n == x || m)

markBoard :: Int -> Board -> Board
markBoard x b =
    let markRow row = markNumber x <$> row
    in  markRow <$> b

markBoards :: Int -> [Board] -> [Board]
markBoards x = fmap $ markBoard x

isMarked :: Number -> Bool
isMarked (Number _ b) = b

checkLine :: [Number] -> Bool
checkLine l = length (filter isMarked l) == 5

check :: Board -> Bool
check rows =
    let columns = transpose rows
        checkLines xs = any id (fmap checkLine xs)
    in  checkLines rows || checkLines columns

sumNumbers :: [Number] -> Int
sumNumbers (Number n _:ns) = n + sumNumbers ns
sumNumbers [] = 0

sumBoard :: Board -> Int
sumBoard board =
    let isUnmarked (Number _ m) = not m
        unmarkedRow = filter isUnmarked
        sumBoardRow = sumNumbers . unmarkedRow
    in  sum $ sumBoardRow <$> board

won :: (Int, Board, Bool) -> Bool
won (_, _, w) = w

inform :: Int -> Board -> (Int, Board, Bool)
inform call board = (call, board, check board)

removeWon :: (Int, Board, Bool) -> (Int, Board)
removeWon (c, b, _) = (c, b)

findWinners :: [Int] -> [Board] -> [(Int, Board)]
findWinners calls boards =
    let go (call:calls') boards' wins =
            let marked   = markBoards call boards'
                checked  = inform call <$> marked
                nextWins = filter won checked
                stillNot = bored <$> filter (not . won) checked
            in
                go calls' stillNot (wins <> nextWins)
        go [] _ wins = wins
    in removeWon <$> go calls boards []

score :: Int -> Board -> Int
score x b = x * sumBoard b

part1 :: ([Int], [[[Int]]]) -> Int
part1 (calls, boards) =
    let boards' = initBoard <$> boards
    in  uncurry score $ head $ findWinners calls boards'

bored :: (Int, Board, Bool) -> Board
bored (_, b, _) = b

part2 :: ([Int], [[[Int]]]) -> Int
part2 (calls, boards) =
    let boards' = initBoard <$> boards
    in uncurry score $ last $ findWinners calls boards'

main :: IO ()
main = do
    input <- get "2021-12-04.txt"
    put parseInput part1 input
    put parseInput part2 input
