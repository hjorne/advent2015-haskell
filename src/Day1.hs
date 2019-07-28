module Day1 where

import System.IO
import Debug.Trace

day1 :: IO () 
day1 = print "Executing day 1" >> part1 >> part2

part1 :: IO ()
part1 = readFile "input/day1.txt" >>= print . solution1

part2 :: IO ()
part2 = readFile "input/day1.txt" >>= print . solution2
        
solution1 :: String -> Int
solution1 = finalFloor . stringToInts

parenToInt :: Char -> Int
parenToInt '(' = 1
parenToInt ')' = -1

stringToInts :: String -> [Int]
stringToInts = fmap parenToInt

finalFloor :: [Int] -> Int
finalFloor = sum

countToBasement :: [Int] -> Int
countToBasement = snd . foldl go (0, 0)
    where go (floor, index) dfloor
            | floor == -1 = (floor, index)
            | otherwise   = (floor + dfloor, index + 1)

solution2 :: String -> Int
solution2 = countToBasement . stringToInts
