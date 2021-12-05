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
type Bingo = Bool

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

allMarked :: [Number] -> Bingo
allMarked = (== 5) . length . filter isMarked

anyLine :: Board -> Bingo
anyLine = or . fmap allMarked

bingo :: Board -> Bingo
bingo rows =
    let columns = transpose rows
    in  anyLine rows || anyLine columns

check :: Call -> Board -> ((Call, Board), Bingo)
check call board = ((call, board), bingo board)

hasBingo :: ((Call, Board), Bingo) -> Bingo
hasBingo = snd

stripBingo :: ((Call, Board), Bingo) -> (Call, Board)
stripBingo = fst

bored :: ((Call, Board), Bingo) -> Board
bored = snd . fst

bingos :: Calls -> [Board] -> [(Call, Board)]
bingos allCalls allBoards =
    let go (call:calls) notYets hasBingos =
            let marked    = markBoards call notYets
                checked   = check call <$> marked
                newBingos = stripBingo <$> filter hasBingo checked
                stillNots = bored <$> filter (not . hasBingo) checked
            in
                go calls stillNots $ hasBingos <> newBingos
        go [] _ hasBingos = hasBingos
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
    uncurry score $ head $ bingos calls boards

part2 :: (Calls, [Board]) -> Int
part2 (calls, boards) =
    uncurry score $ last $ bingos calls boards

main :: IO ()
main = do
    input <- get "2021-12-04.txt"
    put parseInput part1 input
    put parseInput part2 input
