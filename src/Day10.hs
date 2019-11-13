module Day10 () where

input :: String
input = "1321131112"

compress :: String -> [(Int, Char)]
compress [] = []
compress cs = (length prefix, head cs) : compress suffix
    where (prefix, suffix) = span (== head cs) cs

explode :: [(Int, Char)] -> String
explode [] = []
explode ((x, y):xs) = show x ++ [y] ++ explode xs

lookandsay :: String -> String
lookandsay = explode . compress

part1 :: Int
part1 = length $ iterate lookandsay input !! 40

part2 :: Int
part2 = length $ iterate lookandsay input !! 50