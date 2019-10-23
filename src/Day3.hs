module Day3 where

import qualified Data.Set as S
import Prelude hiding (Left, Right)

data Location = Location Int Int deriving (Eq, Show, Ord)
data Direction = Up | Down | Left | Right deriving (Eq, Show)

day3 :: IO ()
day3 = print "Executing day3" >> part1 >> part2

part1 :: IO ()
part1 = 
    readFile "input/day3.txt" >>=
    print . S.size . walk . strToDirs

part2 :: IO ()
part2 =
    readFile "input/day3.txt" >>=
    print . doubleWalk . strToDirs

strToDirs :: String -> [Direction]
strToDirs = map f
    where f '^' = Up
          f '<' = Left
          f '>' = Right
          f 'v' = Down
          f _   = error "Unknown character in direction list"

move :: Direction -> Location -> Location
move Up    (Location x y) = Location x (y + 1)
move Down  (Location x y) = Location x (y - 1)
move Left  (Location x y) = Location (x - 1) y
move Right (Location x y) = Location (x + 1) y

everyOther :: [a] -> ([a], [a])
everyOther [] = ([], [])
everyOther (x:y:xs) = (x:xs', y:ys')
    where (xs', ys') = everyOther xs

doubleWalk :: [Direction] -> Int
doubleWalk ds = S.size $ S.union (walk d1) (walk d2)
    where (d1, d2) = everyOther ds

walk :: [Direction] -> S.Set Location
walk steps = walk' steps origin (S.singleton origin)
    where walk' :: [Direction] -> Location -> S.Set Location -> S.Set Location
          walk' [] _ visited = visited
          walk' (d:ds) loc visited = 
            let loc' = move d loc
            in  walk' ds loc' (S.insert loc' visited)

          origin = Location 0 0