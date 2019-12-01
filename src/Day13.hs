{-# LANGUAGE TupleSections #-}

module Day13 (day13) where

import           Prelude                 hiding ( lines )
import qualified Data.Map                      as M
import           Data.Map                       ( (!) )
import           Data.List                      ( nub
                                                , permutations
                                                )
import           Data.Maybe                     ( fromJust )
import           Text.Trifecta
import           Control.Applicative

type ID = String
type Map = M.Map (ID, ID) Integer

day13 :: IO ()
day13 = parseFromFile file "input/day13.txt"
  >>= \g -> print (part1 <$> g) >> print (part2 <$> g)

file = M.fromList <$> some lines
lines =
  token
    $   mkEdge
    <$> word
    <*> word
    <*> word
    <*> integer
    <*> count 6 word
    <*> word
    <*> dot
word = token (some letter)
mkEdge n1 _ gain v _ n2 _ = ((n1, n2), v' v gain)
 where
  v' v "lose" = -v
  v' v "gain" = v

part1 :: Map -> Integer
part1 m = maximum . fmap (value m) . permutations . nodes $ m

nodes :: Map -> [ID]
nodes g = nub $ fst <$> M.keys g

value :: Map -> [ID] -> Integer
value m xs = sum $ (m !) <$> pairs xs

pairs :: [a] -> [(a, a)]
pairs xs = pairs' (head xs) xs
 where
  pairs' h [x         ] = [(x, h), (h, x)]
  pairs' h (x : y : xs) = (x, y) : (y, x) : pairs' h (y : xs)

pairsSingle :: a -> [a] -> [(a, a)]
pairsSingle x xs = fmap (x, ) xs ++ fmap (, x) xs

part2 :: Map -> Integer
part2 m = part1 (M.union m m')
 where
  newPairs = pairsSingle "Joe" (nodes m)
  m'       = M.fromList $ zip newPairs (repeat 0)


