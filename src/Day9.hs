module Day9 where

import Text.Trifecta
import Data.Maybe (fromJust)
import Data.Map ((!))
import qualified Data.Map as M
import Data.List (nub)
import Data.Sequence (Seq)
import Debug.Trace
import qualified Data.Sequence as S

type ID = String
type Edge = (ID, ID)
type Graph = M.Map Edge Integer

infile :: String
infile = "input/day9.txt"

day9 :: IO ()
day9 = parseFromFile (some parseLine) infile >>= 
       \dists -> 
       print (part1 $ fromJust dists) >>
       print (part2 $ fromJust dists)


part0 :: [(Edge, Integer)] -> [Integer]
part0 dists = fmap (sum . fmap (g !) . pairs) . permutations . nodes $ g
    where g = M.fromList . doubleE $ dists

part1 :: [(Edge, Integer)] -> Integer
part1 = minimum . part0

part2 :: [(Edge, Integer)] -> Integer
part2 = maximum . part0

pairs :: [a] -> [(a, a)]          
pairs [x, y] = [(x, y)]
pairs (x:y:xs) = (x, y) : pairs (y:xs)
pairs _ = error "Cannot call pairs on fewer than 1 argument"

parseLine :: Parser (Edge, Integer)
parseLine = mkEdge <$> parseID <*> symbol "to" <*> parseID <*> symbolic '=' <*> natural
    where mkEdge id1 _ id2 _ dist = ((id1, id2), dist)
          parseID = token $ some letter

doubleE :: [(Edge, Integer)] -> [(Edge, Integer)]
doubleE = foldr go [] 
    where go x acc = x : swap x : acc
          swap ((id1, id2), dist) = ((id2, id1), dist)

nodes :: Graph -> [ID]
nodes = nub . fmap fst . M.keys

prefixes :: [a] -> [(a, [a])]
prefixes xs = dropcopy <$> zip xs [0..]
    where dropcopy (x, i) = (x, deleteL i xs)

deleteL :: Int -> [a] -> [a]
deleteL i xs = left ++ tail right
    where (left, right) = splitAt i xs

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations s = p >>= gen
    where p = prefixes s
          gen (pre, suf) = (pre:) <$> permutations suf