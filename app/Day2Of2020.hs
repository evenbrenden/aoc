module Day2Of2020 where

import System.IO
import Text.Trifecta

data Password =
    Password { indexes :: (Int, Int), character :: Char, password :: String }
    deriving Show

parsePassword :: Parser Password
parsePassword = do
    firstIndex <- fromIntegral <$> integer
    char '-'
    secondIndex <- fromIntegral <$> integer
    character' <- anyChar
    char ':'
    space
    password' <- some $ noneOf "\n"
    char '\n'
    return $ Password { indexes = (firstIndex, secondIndex), character = character', password = password' }

parsePasswords:: Parser [Password]
parsePasswords = do
    ps <- many parsePassword
    eof
    return ps

passwords :: String -> Result [Password]
passwords = parseString parsePasswords mempty

handleResult (Success is) = is
handleResult (Failure f) = error $ show f

parse :: String -> [Password]
parse = handleResult . passwords

charCount :: Char -> String -> Int
charCount character = length . filter (== character)

isValidPart1 :: Password -> Bool
isValidPart1 p =
    let start = (fst . indexes) p
        end = (snd . indexes) p
        numHits = charCount (character p) (password p)
    in numHits >= start && numHits <= end

part1 :: [Password] -> Int
part1 = length . filter isValidPart1

isValidPart2 :: Password -> Bool
isValidPart2 p =
    let firstIndex = ((fst . indexes) p) - 1
        secondIndex = ((snd . indexes) p) - 1
    in (password p !! firstIndex == character p && password p !! secondIndex /= character p) ||
       (password p !! firstIndex /= character p && password p !! secondIndex == character p)

part2 :: [Password] -> Int
part2 = length . filter isValidPart2

solve :: IO ()
solve = do
    handle <- openFile "app/Day2Of2020.txt" ReadMode
    contents <- hGetContents handle
    print $ part1 . parse $ contents
    print $ part2 . parse $ contents
