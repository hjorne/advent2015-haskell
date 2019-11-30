module Day15 where

import           Prelude                 hiding ( lines )
import           Text.Trifecta           hiding ( dot )
import           Data.List                      ( transpose )

infile :: String
infile = "input/day15.txt"

n :: Integer
n = 100

day15 :: IO ()
day15 = parseFromFile file infile >>= \ingredients ->
    print (part1 <$> ingredients) >> print (part2 <$> ingredients)

file :: Parser [[Integer]]
file = some lines
lines =
    fmap reverse $ some letter *> string ": " *> ingredient `sepBy` string ", "
ingredient = some letter *> space *> integer

cs :: [[Integer]]
cs =
    [ [c1, c2, c3, n - c1 - c2 - c3]
    | c1 <- [0 .. n]
    , c2 <- [0 .. (n - c1)]
    , c3 <- [0 .. (n - c2)]
    , n - c1 - c2 - c3 >= 0
    ]

part1 :: [[Integer]] -> Integer
part1 is = maximum . fmap (score nocals) $ cs
    where nocals = fmap tail is

part2 :: [[Integer]] -> Integer
part2 is = maximum . fmap (filteredScore is) $ cs

score :: [[Integer]] -> [Integer] -> Integer
score is weights =
    zeroProduct . fmap sum . transpose $ fmap mult weights `zipApp` is

filteredScore :: [[Integer]] -> [Integer] -> Integer
filteredScore is weights
    | calories == 500 = zeroProduct . fmap sum $ tail ingredientRows
    | otherwise       = 0
  where
    weightedIngredients = fmap mult weights `zipApp` is
    ingredientRows      = transpose weightedIngredients
    calories            = sum (head ingredientRows)


mult :: Integer -> [Integer] -> [Integer]
mult a = fmap (* a)

zeroProduct :: [Integer] -> Integer
zeroProduct xs | any (<= 0) xs = 0
               | otherwise     = product xs

zipApp :: [a -> b] -> [a] -> [b]
zipApp fs xs = foldr go [] (zip fs xs) where go (f, x) xs = f x : xs
