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

sumUnmarked :: Board -> Int
sumUnmarked board =
    let isUnmarked (Number _ m) = not m
        unmarkedRow = filter isUnmarked
        sumUnmarkedRow = sumNumbers . unmarkedRow
    in  sum $ sumUnmarkedRow <$> board

won :: (Int, Board, Bool) -> Bool
won (_, _, w) = w

inform :: a -> Board -> (a, Board, Bool)
inform call board = (call, board, check board)

go :: [Int] -> [Board] -> (Int, Board, Bool)
go (call:calls) boards =
    let marked  = markBoards call boards
        checked = inform call <$> marked
        wins    = filter won checked
    in
        if length wins > 0 then
            head wins
        else
            go calls marked
go [] _ = error "no winners"

findWinner :: [Int] -> [Board] -> (Int, Board)
findWinner calls boards =
    let (call, board, _) = go calls boards
    in  (call, board)

score :: Int -> Board -> Int
score x b = x * sumUnmarked b

part1 :: ([Int], [[[Int]]]) -> Int
part1 (calls, boards) =
    let boards' = initBoard <$> boards
        (call, board) = findWinner calls boards'
    in  score call board

bored :: (Int, Board, Bool) -> Board
bored (_, b, _) = b

og :: [Int] -> [Board] -> [(Int, Board, Bool)] -> (Int, Board, Bool)
og (call:calls) boards lastWins =
    let marked   = markBoards call boards
        checked  = inform call <$> marked
        nextWins = filter won checked
        stillNot = bored <$> filter (not . won) checked
    in
        if length nextWins > 0 then
            og calls stillNot nextWins
        else
            og calls stillNot lastWins
og [] _ wins = last wins

findLastWinner :: [Int] -> [Board] -> (Int, Board)
findLastWinner calls boards =
    let (call, board, _) = og calls boards []
    in  (call, board)

part2 :: ([Int], [[[Int]]]) -> Int
part2 (calls, boards) =
    let boards' = initBoard <$> boards
        (call, board) = findLastWinner calls boards'
    in  score call board

main :: IO ()
main = do
    input <- get "2021-12-04.txt"
    put parseInput part1 input
    put parseInput part2 input
