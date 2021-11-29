module Day1Of2020 where

import Control.Monad
import System.IO
import Text.Trifecta

int :: Parser Int
int = fromIntegral <$> integer

allInts :: Parser [Int]
allInts = do
    is <- many int
    eof
    return is

parseAllInts :: String -> Result [Int]
parseAllInts = parseString allInts mempty

handleResult (Success is) = is
handleResult (Failure f) = error $ show f

parse :: String -> [Int]
parse = handleResult . parseAllInts

part1 :: [Int] -> Int
part1 ints = head $ do
    x <- ints
    y <- ints
    guard $ x + y == 2020
    return $ x * y

part2 :: [Int] -> Int
part2 ints = head $ do
    x <- ints
    y <- ints
    z <- ints
    guard $ x + y + z == 2020
    return $ x * y * z

solve :: IO ()
solve = do
    handle <- openFile "app/Day1Of2020.txt" ReadMode
    contents <- hGetContents handle
    print $ part1 . parse $ contents
    print $ part2 . parse $ contents
