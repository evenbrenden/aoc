#! /usr/bin/env nix-shell
#! nix-shell -i "ghcid -c 'ghci -Wall -Wno-unused-do-bind' -T main"

module Main where

import Common
import Text.Trifecta

data Password = Password
    { firstIndex  :: Int
    , secondIndex :: Int
    , character   :: Char
    , password    :: String
    } deriving Show

parsePassword :: Parser Password
parsePassword = do
    firstIndex' <- fromIntegral <$> integer
    char '-'
    secondIndex' <- fromIntegral <$> integer
    character' <- anyChar
    char ':'
    space
    password' <- some $ noneOf "\n"
    char '\n'
    return $ Password
        { firstIndex  = firstIndex'
        , secondIndex = secondIndex'
        , character   = character'
        , password    = password' }

parsePasswords:: Parser [Password]
parsePasswords = do
    ps <- many parsePassword
    eof
    return ps

charCount :: Char -> String -> Int
charCount character' = length . filter (== character')

unsafeIsValidPart1 :: Password -> Bool
unsafeIsValidPart1 p =
    let start   = firstIndex p
        end     = secondIndex p
        numHits = charCount (character p) (password p)
    in  numHits >= start && numHits <= end

part1 :: [Password] -> Int
part1 = length . filter unsafeIsValidPart1

unsafeIsValidPart2 :: Password -> Bool
unsafeIsValidPart2 p =
    let firstIndex'  = (firstIndex p) - 1
        secondIndex' = (secondIndex p) - 1
    in  (password p !! firstIndex' == character p && password p !! secondIndex' /= character p) ||
        (password p !! firstIndex' /= character p && password p !! secondIndex' == character p)

part2 :: [Password] -> Int
part2 = length . filter unsafeIsValidPart2

main :: IO ()
main = do
    input <- get "2020-12-02.txt"
    put parsePasswords part1 input
    put parsePasswords part2 input
