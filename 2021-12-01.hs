#! /usr/bin/env nix-shell
#! nix-shell -i "ghcid -c 'ghci -Wall -Wno-unused-do-bind' -T main"

module Main where

import Common
import Text.Trifecta

parseInt :: Parser Int
parseInt = fromIntegral <$> integer

parseInts :: Parser [Int]
parseInts = do
    is <- many parseInt
    eof
    return is

pairs :: [a] -> [(a, a)]
pairs xs = zip xs $ tail xs

pairIncrease :: Ord a => (a, a) -> Bool
pairIncrease (x, y) = x < y

part1 :: [Int] -> Int
part1 = length . filter pairIncrease . pairs

windows :: [a] -> [(a, a, a)]
windows xs = zip3 xs (tail xs) ((tail . tail) xs)

windowPairs :: [a] -> [((a, a, a), (a, a, a))]
windowPairs = pairs . windows

windowIncrease :: (Num a, Ord a) => ((a, a, a), (a, a, a)) -> Bool
windowIncrease ((u, v, w), (x, y, z)) = u + v + w < x + y + z

part2 :: [Int] -> Int
part2 = length . filter windowIncrease . windowPairs

main :: IO ()
main = do
    input <- get "2021-12-01.txt"
    put parseInts part1 input
    put parseInts part2 input
