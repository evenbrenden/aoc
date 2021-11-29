module Day2Of2020 where

import Control.Monad
import System.IO
import Text.Trifecta

data Password =
    Password { range :: (Int, Int), mandatory :: Char, password :: String }
    deriving Show

parsePassword :: Parser Password
parsePassword = do
    start <- fromIntegral <$> integer
    char '-'
    end <- fromIntegral <$> integer
    mandatory' <- anyChar
    char ':'
    space
    password' <- some $ noneOf "\n"
    char '\n'
    return $ Password { range = (start, end), mandatory = mandatory', password = password' }

passwords:: Parser [Password]
passwords = do
    ps <- many parsePassword
    eof
    return ps

parsePasswords :: String -> Result [Password]
parsePasswords = parseString passwords mempty

handleResult (Success is) = is
handleResult (Failure f) = error $ show f

parse :: String -> [Password]
parse = handleResult . parsePasswords

count' :: Char -> String -> Int
count' char' = length . filter (== char')

isValid :: Password -> Bool
isValid p =
    let mandatory' = mandatory p
        start = fst . range $ p
        end = snd . range $ p
        password' = password p
        count'' = count' mandatory' password'
    in count'' >= start && count'' <= end

part1 :: [Password] -> Int
part1 = length . filter isValid

solve :: IO ()
solve = do
    handle <- openFile "app/Day2Of2020.txt" ReadMode
    contents <- hGetContents handle
    print $ part1 . parse $ contents
