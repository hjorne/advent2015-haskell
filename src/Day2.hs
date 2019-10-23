module Day2 (day2) where

import Common (split)
import Data.List (sort)

day2 :: IO () 
day2 = print "Executing day 2" >> part1 >> part2

part1 :: IO ()
part1 = 
    readFile "input/day2.txt" >>= 
    print . 
    sum .
    map (wrappingPaper . dimsToInt) . 
    lines

part2 :: IO ()
part2 = 
    readFile "input/day2.txt" >>=
    print . 
    sum .
    map (ribbon . dimsToInt) . 
    lines 

toInt :: String -> Int
toInt = read

surfaceArea :: [Int] -> Int
surfaceArea [l, w, h] = 2*l*w + 2*w*h + 2*h*l 

volume :: [Int] -> Int
volume [l, w, h] = l*w*h

minPerimeter :: [Int] -> Int
minPerimeter xs = 2*x + 2*y
    where (x, y) = doubleMin xs

extra :: [Int] -> Int
extra xs = x * y
    where (x, y) = doubleMin xs

doubleMin :: [Int] -> (Int, Int)
doubleMin xs = (x, y)
    where (x:y:xs') = sort xs

wrappingPaper :: [Int] -> Int
wrappingPaper xs = surfaceArea xs + extra xs

ribbon :: [Int] -> Int
ribbon xs = minPerimeter xs + volume xs

dimsToInt :: String -> [Int]
dimsToInt = map toInt . split "x"