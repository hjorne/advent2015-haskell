module Day11 () where

import Prelude hiding (String, length)
import Data.Sequence 

type String = Seq Char

input :: String
input = fromList "cqjxjnds"

part1 :: String
part1 = findPass input

part2 :: String
part2 = findPass . iter $ part1

findPass :: String -> String
findPass = until legal iter

iter :: String -> String
iter Empty = Empty
iter (s :|> 'z') = iter s |> 'a'
iter (s :|> c) = s |> succ c

legal :: String -> Bool
legal s = ascending s && checkiol s && overlap s

ascending :: String -> Bool
ascending xs | length xs < 3 = False
ascending (x:<|y:<|z:<|xs) 
    | succ (succ x) == succ y && succ y == z = True
    | otherwise = ascending (y <| z <| xs)

checkiol :: String -> Bool
checkiol Empty = True
checkiol (x :<| xs) 
    | x `elem` illegal = False
    | otherwise = checkiol xs
    where illegal = ['i', 'o', 'l']

countPair :: String -> Int
countPair Empty = 0
countPair (_ :<| Empty) = 0
countPair (x :<| y :<| xs) 
    | x == y = 1 + countPair xs
    | otherwise = countPair (y <| xs)

overlap :: String -> Bool
overlap s = countPair s > 1
