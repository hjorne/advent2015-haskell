{-# LANGUAGE ViewPatterns #-}

module Day16 where

import           Prelude                 hiding ( lines )
import           Text.Trifecta
import qualified Data.Map                      as M

type Sue = M.Map String Integer

infile :: Integer -> String
infile x = "input/day16." ++ show x ++ ".txt"

day16 :: IO ()
day16 = parseFromFile file1 (infile 1) >>= \originalSue ->
    parseFromFile file2 (infile 2)
        >>= \sueList -> print (part1 <$> originalSue <*> sueList)

file1 = M.fromList <$> some object
file2 = some lines
lines =
    mkSue
        <$>     string "Sue "
        <*>     integer
        <*>     string ": "
        <*>     object
        `sepBy` string ", "
    where mkSue _ i _ objs = (i, M.fromList objs)
object = mktuple <$> some letter <*> string ": " <*> integer
    where mktuple x _ y = (x, y)

part1 :: Sue -> [(Integer, Sue)] -> Integer
part1 trueSue (unzip -> (is, sues)) =
    fst . head . filter (checkSue . snd) $ zip is $fmap (sueDiff trueSue) sues

sueDiff :: Sue -> Sue -> Sue
sueDiff = M.intersectionWith (-) 

checkSue :: Sue -> Bool
checkSue = all (==0) . M.elems 
