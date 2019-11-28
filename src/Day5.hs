module Day5 (day5) where

import qualified Data.Set as S
import Data.List (isInfixOf)

day5 :: IO ()
day5 = print "Day 5: " >> part1 >> part2

part1 :: IO ()
part1 = 
    readFile "input/day5.txt" >>=
    print . length . filter nice . lines

part2 :: IO ()
part2 = 
    readFile "input/day5.txt" >>=
    print . length . filter nice2 . lines

vowels :: S.Set Char
vowels = S.fromList ['a', 'e', 'i', 'o', 'u']

illegal :: String -> Bool
illegal x = not . or $ fmap (`isInfixOf` x) ["ab", "cd", "pq", "xy"]

threeVowels :: String -> Bool
threeVowels = (>=3) . length . filter (`S.member` vowels) 

twiceInARow :: String -> Bool
twiceInARow [x, y] = x == y
twiceInARow (x:y:xs) = x == y || twiceInARow (y:xs)

nice :: String -> Bool
nice x = illegal x && twiceInARow x && threeVowels x

duplicateStep :: Eq a => [a] -> Bool
duplicateStep xs | length xs < 3 = False
duplicateStep [x, _, y] = x == y
duplicateStep (x:y:z:xs) = x == z || duplicateStep (y:z:xs)

pairs :: [a] -> [[a]]
pairs xs@[x, y] = [xs]
pairs (x:y:xs) = [x, y] : pairs (y:xs)

appearsTwice :: [String] -> Bool
appearsTwice [x, y] = False
appearsTwice (x:y:xs) = x `elem` xs || appearsTwice (y:xs)

nice2 :: String -> Bool
nice2 x = duplicateStep x && appearsTwice  (pairs x)
