#! /usr/bin/env nix-shell
#! nix-shell -i "ghcid -c 'ghci -Wall -Wno-unused-do-bind' -T main"

module Main where

import Common
import Control.Applicative
import Data.List
import Text.Trifecta

type Number = [Bool]
type Numbers = [Number]
type Comparator = Number -> Bool
type Filter = Numbers -> Numbers

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

parseNumber:: Parser Number
parseNumber = do
    bits <- some parseBool
    char '\n'
    return bits

parseNumbers :: Parser Numbers
parseNumbers = do
    bitsList <- many parseNumber
    eof
    return bitsList

mostCommon :: Number -> Bool
mostCommon bools =
    let numTrues = length . filter (==True) $ bools
        numFalses = length . filter (==False) $ bools
    in  numTrues >= numFalses

pow2 :: Bool -> Int -> Int
pow2 True = (^) 2
pow2 False = const 0

int :: Number -> Int
int num = sum $ uncurry pow2 <$> zip (reverse num) [0..]

gamma :: Numbers -> Number
gamma = fmap mostCommon . transpose

epsilon :: Numbers -> Number
epsilon = fmap not . gamma

part1 :: Numbers -> Int
part1 nums = (int . gamma) nums * (int . epsilon) nums

notIf :: Bool -> (Bool -> Bool)
notIf True = id
notIf False = not

ratingFilter :: Comparator -> Numbers -> Numbers
ratingFilter comp nums =
    let go xs _ | length xs == 1 = xs
        go xs idx =
            let columns   = transpose xs
                selected  = comp (columns !! idx)
                predicate = notIf selected . (!! idx)
            in  go (filter predicate xs) (idx + 1)
    in  go nums 0

ogrFilter :: Numbers -> Numbers
ogrFilter = ratingFilter mostCommon

csrFilter :: Numbers -> Numbers
csrFilter = ratingFilter $ not . mostCommon

rating :: Filter -> Numbers -> Int
rating filt nums = int . head $ filt nums

ogr :: Numbers -> Int
ogr = rating ogrFilter

csr :: Numbers -> Int
csr = rating csrFilter

part2 :: Numbers -> Int
part2 nums = ogr nums * csr nums

main :: IO ()
main = do
    input <- get "2021-12-03.txt"
    put parseNumbers part1 input
    put parseNumbers part2 input
