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
    in  numTrues > numFalses

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

main :: IO ()
main = do
    input <- get "2021-12-03.txt"
    put parseNumbers part1 input
