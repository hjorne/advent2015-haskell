module Day17 where

sizes :: [Integer]
sizes = [20, 15, 10, 5, 5]

total :: Integer
total = 150

day17 :: IO ()
day17 = print $ partition total

partitionSingle :: Integer -> Integer -> Integer
partitionSingle x size 
    | x < size = 0
    | x == size = 1
    | otherwise = partition (x - size)

partition :: Integer -> Integer
partition x = sum $ fmap (partitionSingle x) sizes
