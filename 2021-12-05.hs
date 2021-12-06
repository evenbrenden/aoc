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
    if x1 == x2 then
        return $ Just [(x1, y) | y <- [min y1 y2.. max y1 y2]]
    else if y1 == y2 then
        return $ Just [(x, y1) | x <- [min x1 x2.. max x1 x2]]
    else if includeDiagonals && abs (x2 - x1) == abs (y2 - y1) then
        if x1 < x2 && y1 < y2 then
            return $ Just [(x1 + i, y1 + i) | i <- [0..abs (x2 - x1)]]
        else if x1 > x2 && y1 < y2 then
            return $ Just [(x1 - i, y1 + i) | i <- [0..abs (x2 - x1)]]
        else if x1 > x2 && y1 > y2 then
            return $ Just [(x1 - i, y1 - i) | i <- [0..abs (x2 - x1)]]
        else
            return $ Just [(x1 + i, y1 - i) | i <- [0..abs (x2 - x1)]]
    else
        return Nothing

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
