#! /usr/bin/env nix-shell
#! nix-shell -i "ghcid -c 'ghci -Wall -Wno-unused-do-bind' -T main"

module Main where

import Common
import Control.Applicative
import Data.List
import Text.Trifecta

parseTrue :: Parser Bool
parseTrue = do
    char '1'
    return True

parseFalse :: Parser Bool
parseFalse = do
    char '0'
    return False

parseBool :: Parser Bool
parseBool = parseTrue <|> parseFalse

parseNumber:: Parser [Bool]
parseNumber = do
    bits <- some parseBool
    char '\n'
    return bits

parseNumbers :: Parser [[Bool]]
parseNumbers = do
    bitsList <- many parseNumber
    eof
    return bitsList

mostCommon :: [Bool] -> Bool
mostCommon bools =
    let numTrues = length . filter (==True) $ bools
        numFalses = length . filter (==False) $ bools
    in  numTrues >= numFalses

pow2 :: Bool -> Int -> Int
pow2 True = (^) 2
pow2 False = const 0

int :: [Bool] -> Int
int num = sum $ uncurry pow2 <$> zip (reverse num) [0..]

gamma :: [[Bool]] -> [Bool]
gamma = fmap mostCommon . transpose

epsilon :: [[Bool]] -> [Bool]
epsilon = fmap not . gamma

part1 :: [[Bool]] -> Int
part1 nums = (int . gamma) nums * (int . epsilon) nums

notIf :: Bool -> (Bool -> Bool)
notIf True = id
notIf False = not

ogrFilter :: [[Bool]] -> Int -> [[Bool]]
ogrFilter nums _ | length nums == 1 = nums
ogrFilter nums idx =
    let cols  = transpose nums
        most  = mostCommon $ cols !! idx
        predi = notIf most . (!! idx)
    in  ogrFilter (filter predi nums) (idx + 1)

ogr :: [[Bool]] -> Int
ogr nums = int . head $ ogrFilter nums 0

co2Filter :: [[Bool]] -> Int -> [[Bool]]
co2Filter nums _ | length nums == 1 = nums
co2Filter nums idx =
    let cols  = transpose nums
        least = not . mostCommon $ cols !! idx
        predi  = notIf least . (!! idx)
    in  co2Filter (filter predi nums) (idx + 1)

co2 :: [[Bool]] -> Int
co2 nums = int . head $ co2Filter nums 0

part2 :: [[Bool]] -> Int
part2 nums = ogr nums * co2 nums

main :: IO ()
main = do
    input <- get "2021-12-03.txt"
    put parseNumbers part1 input
    put parseNumbers part2 input
