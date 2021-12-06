#! /usr/bin/env nix-shell
#! nix-shell -i "ghcid -c 'ghci -Wall -Wno-unused-do-bind' -T main"

module Main where

import Common
import qualified Data.Map as M
import Data.Maybe
import Text.Trifecta

type Point = (Int, Int)
type Line = [Point]
type Value = Int

parseInt :: Parser Int
parseInt = fromInteger <$> integer

isVertical :: Point -> Point -> Bool
isVertical (x1, _) (x2, _) =
    x1 == x2

isHorizontal :: Point -> Point -> Bool
isHorizontal (_, y1) (_, y2) =
    y1 == y2

isDiagonal :: Point -> Point -> Bool
isDiagonal (x1, y1) (x2, y2) =
    abs (x2 - x1) == abs (y2 - y1)

generateVertical :: Point -> Point -> Line
generateVertical (x1, y1) (_, y2) =
    [(x1, y) | y <- [min y1 y2..max y1 y2]]

generateHorizontal :: Point -> Point -> Line
generateHorizontal (x1, y1) (x2, _) =
    [(x, y1) | x <- [min x1 x2..max x1 x2]]

range :: Int -> Int -> [Int]
range from to
  | from < to = [from..to]
  | otherwise = [from, (from - 1)..to]

generateDiagonal :: Point -> Point -> Line
generateDiagonal (x1, y1) (x2, y2) =
    zip (range x1 x2) (range y1 y2)

maybeLine :: Bool -> Point -> Point -> Maybe Line
maybeLine includeDiagonals p1 p2
    | isVertical p1 p2 =
        Just $ generateVertical p1 p2
    | isHorizontal p1 p2 =
        Just $ generateHorizontal p1 p2
    | includeDiagonals && isDiagonal p1 p2 =
        Just $ generateDiagonal p1 p2
    | otherwise = Nothing

parseLine :: Bool -> Parser (Maybe Line)
parseLine includeDiagonals = do
    x1 <- parseInt
    char ','
    y1 <- parseInt
    string "->"
    space
    x2 <- parseInt
    char ','
    y2 <- parseInt
    let p1 = (x1, y1)
    let p2 = (x2, y2)
    return $ maybeLine includeDiagonals p1 p2

parseLines :: Bool -> Parser [Line]
parseLines includeDiagonals = do
    ls <- many (parseLine includeDiagonals)
    eof
    return $ catMaybes ls

increment :: Point -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
increment = flip (M.insertWith (+)) 1

plot :: [Line] -> M.Map (Int, Int) Int
plot = foldr increment M.empty . concat

part1 :: [Line] -> Int
part1 = length . M.filter (> 1) . plot

part2 :: [Line] -> Int
part2 = part1

main :: IO ()
main = do
    input <- get "2021-12-05.txt"
    put (parseLines False) part1 input
    put (parseLines True) part2 input
