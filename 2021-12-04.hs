#! /usr/bin/env nix-shell
#! nix-shell -i "ghcid -c 'ghci -Wall -Wno-unused-do-bind' -T main"

module Main where

import Common
import Data.List
import Text.Trifecta

type Mark = Bool
data Number = Number Int Mark deriving Show
type Board = [[Number]]
type Call = Int
type Calls = [Call]

parseCalls :: Parser Calls
parseCalls = do
    calls <- integer `sepBy` (char ',')
    return $ fromInteger <$> calls

unMarked :: Mark
unMarked = False

parseRow :: Parser [Number]
parseRow = do
    a <- integer
    b <- integer
    c <- integer
    d <- integer
    e <- integer
    return $ flip Number unMarked . fromIntegral <$> [a, b, c, d, e]

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

markNumber :: Call -> Number -> Number
markNumber call (Number n m) = Number n (n == call || m)

markBoard :: Call -> Board -> Board
markBoard call board =
    let markRow row = markNumber call <$> row
    in  markRow <$> board

markBoards :: Call -> [Board] -> [Board]
markBoards = fmap . markBoard

isMarked :: Number -> Mark
isMarked (Number _ b) = b

allMarked :: [Number] -> Bool
allMarked = (== 5) . length . filter isMarked

anyLine :: Board -> Bool
anyLine = or . fmap allMarked

check :: Board -> Bool
check rows =
    let columns = transpose rows
    in  anyLine rows || anyLine columns

inform :: Call -> Board -> ((Call, Board), Bool)
inform call board = ((call, board), check board)

won :: ((Call, Board), Bool) -> Bool
won = snd

noWon :: ((Call, Board), Bool) -> (Call, Board)
noWon = fst

bored :: ((Call, Board), Bool) -> Board
bored = snd . fst

winners :: Calls -> [Board] -> [(Call, Board)]
winners allCalls allBoards =
    let go (call:calls) notYets wins =
            let marked    = markBoards call notYets
                checked   = inform call <$> marked
                newWins   = noWon <$> filter won checked
                stillNots = bored <$> filter (not . won) checked
            in
                go calls stillNots $ wins <> newWins
        go [] _ wins = wins
    in  go allCalls allBoards []

number :: Number -> Int
number (Number n _) = n

sumUnmarked :: Board -> Int
sumUnmarked =
    let unmarkedRow    = filter $ not . isMarked
        sumUnmarkedRow = sum . fmap number . unmarkedRow
    in  sum . fmap sumUnmarkedRow

score :: Int -> Board -> Int
score call board = call * sumUnmarked board

part1 :: (Calls, [Board]) -> Int
part1 (calls, boards) =
    uncurry score $ head $ winners calls boards

part2 :: (Calls, [Board]) -> Int
part2 (calls, boards) =
    uncurry score $ last $ winners calls boards

main :: IO ()
main = do
    input <- get "2021-12-04.txt"
    put parseInput part1 input
    put parseInput part2 input
