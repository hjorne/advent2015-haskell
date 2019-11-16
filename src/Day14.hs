{-# LANGUAGE ViewPatterns #-}

module Day14 where

type Name = String
type Distance = Int
type Time = Int
data Deer = Deer Name Distance Time Time deriving (Eq, Show)

infile :: String
infile = "input/day14.txt"

day14 :: IO ()
day14 = readFile infile >>= print . fmap parseLine . lines

parseLine :: String -> Deer
parseLine (words -> [name,"can","fly", distance,
                     "km/s","for",len,"seconds,",
                     "but","then","must","rest",
                     "for",timeout,"seconds."]) = Deer name (read distance) (read len) (read timeout)
