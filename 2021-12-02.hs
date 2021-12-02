#! /usr/bin/env nix-shell
#! nix-shell -i "ghcid -c 'ghci -Wall -Wno-unused-do-bind' -T main"

module Main where

import Common
import Control.Applicative
import Text.Trifecta

data Move =
      Forward Int
    | Down Int
    | Up Int

type Horizontal = Int
type Depth = Int
type Travel = (Horizontal, Depth)
type Aim = Int
data State = State Travel Aim deriving Show

noTravel :: Travel
noTravel = (0, 0)

initState :: State
initState = State noTravel 0

parseForward :: Parser (Int -> Move)
parseForward = do
    string "forward"
    return $ Forward

parseDown :: Parser (Int -> Move)
parseDown = do
    string "down"
    return $ Down

parseUp :: Parser (Int -> Move)
parseUp = do
    string "up"
    return $ Up

parseMove :: Parser Move
parseMove = do
    move <- parseForward <|> parseDown <|> parseUp
    space
    amount <- fromIntegral <$> integer
    return $ move amount

parseMoves:: Parser [Move]
parseMoves = do
    ps <- many parseMove
    eof
    return ps

nextTravel :: Move -> Travel -> Travel
nextTravel (Forward x) (h, d) = (h + x, d)
nextTravel (Down x) (h, d) = (h, d + x)
nextTravel (Up x) (h, d) = (h, d - x)

endTravel :: [Move] -> Travel
endTravel = foldr nextTravel noTravel

multiply :: Travel -> Int
multiply = uncurry (*)

part1 :: [Move] -> Int
part1 = multiply . endTravel

nextState :: State -> Move -> State
nextState (State (h, d) a) (Forward x) = State (h + x, d + a *x) a
nextState (State t a) (Down x) = State t (a + x)
nextState (State t a) (Up x) = State t (a - x)

endState :: [Move] -> State
endState = foldl nextState initState

travel :: State -> Travel
travel (State t _) = t

part2 :: [Move] -> Int
part2 = multiply . travel . endState

main :: IO ()
main = do
    input <- get "2021-12-02.txt"
    put parseMoves part1 input
    put parseMoves part2 input
