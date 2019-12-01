{-# LANGUAGE Strict #-}

module Day18 where

import           Data.Foldable                  ( traverse_
                                                , for_
                                                )
import           Data.Ix                        ( range )
import           Control.Monad.ST
import qualified Data.Array.MArray             as MA
import qualified Data.Array.ST                 as A
import qualified Data.Array.Unboxed            as UA
import Debug.Trace

type R = (Int, Int)
type MArray s = A.STUArray s R Bool

infile :: String
infile = "input/day18.txt"

n :: Int
n = 1

day18 :: IO ()
day18 = readFile infile >>= print . part1 . parse

parse :: String -> [[Bool]]
parse = fmap toBinary . words

part1 :: [[Bool]] -> Int
part1 bss = count $ run bss n

dims :: [[a]] -> (R, R)
dims xss@(xs : _) = ((0, 0), (length xs - 1, length xss - 1))

toBinary :: String -> [Bool]
toBinary = fmap toBinaryC

toBinaryC :: Char -> Bool
toBinaryC '.' = False
toBinaryC '#' = True
toBinaryC _   = error "Unknown character"

toArray :: [[Bool]] -> ST s (MArray s)
toArray bss = MA.newListArray (dims bss) (concat bss)

count :: UA.UArray R Bool -> Int
count = length . filter id . UA.elems 

run :: [[Bool]] -> Int -> UA.UArray R Bool
run bss n = A.runSTUArray $ toArray bss >>= \arr ->
    for_ [1 .. n] (const $ step arr) >> return arr

step :: MArray s -> ST s ()
step arr = MA.getBounds arr >>= \bounds -> traverse_ (stepR arr) (range bounds)

stepR :: MArray s -> R -> ST s ()
stepR arr (x, y) = MA.getBounds arr >>= \(_, (xmax, ymax)) ->
    MA.readArray arr (x, y) >>= \value ->
        traverse (MA.readArray arr) (neighborR xmax ymax) >>= \neighbors ->
            MA.writeArray arr (x, y) (runState value neighbors)
  where
    neighborR xmax ymax =
        [ (x + dx, y + dy)
        | dx <- [-1 .. 1]
        , dy <- [-1 .. 1]
        , abs dx + abs dy == 1
        , x + dx >= 0
        , y + dy >= 0
        , x + dx <= xmax
        , y + dy <= ymax
        ]

runState :: Bool -> [Bool] -> Bool
runState v neighbors | v && (n == 2 || n == 3) = True
                     | v                       = False
                     | not v && n == 3         = True
                     | otherwise               = False
    where n = length . filter id $ neighbors
