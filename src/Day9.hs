module Day9 (day9) where

import Text.Trifecta
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.List (nub)

type ID = String
type Edge = (ID, ID)
type Graph = M.Map Edge Integer

infile :: String
infile = "input/day9.txt"

day9 :: IO ()
day9 = parseFromFile (some parseLine) infile >>= print . permutations . nodes . M.fromList . doubleE . fromJust

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

permutations :: [a] -> [[a]]
permutations [x] = [[x]]
permutations (x:xs) = (x:) <$> permutations xs