module Main where

import System.IO
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

parseInput :: String -> [Password]
parseInput = handleResult . parseString parsePasswords mempty

handleResult (Success is) = is
handleResult (Failure f) = error $ show f

charCount :: Char -> String -> Int
charCount character = length . filter (== character)

isValidPart1 :: Password -> Bool
isValidPart1 p =
    let start   = firstIndex p
        end     = secondIndex p
        numHits = charCount (character p) (password p)
    in  numHits >= start && numHits <= end

part1 :: [Password] -> Int
part1 = length . filter isValidPart1

isValidPart2 :: Password -> Bool
isValidPart2 p =
    let firstIndex'  = (firstIndex p) - 1
        secondIndex' = (secondIndex p) - 1
    in  (password p !! firstIndex' == character p && password p !! secondIndex' /= character p) ||
        (password p !! firstIndex' /= character p && password p !! secondIndex' == character p)

part2 :: [Password] -> Int
part2 = length . filter isValidPart2

main :: IO ()
main = do
    handle <- openFile "app/Day2Of2020.txt" ReadMode
    contents <- hGetContents handle
    print $ part1 . parseInput $ contents
    print $ part2 . parseInput $ contents
