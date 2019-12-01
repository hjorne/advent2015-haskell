{-# LANGUAGE Strict #-}

module Day18 where

import           Data.Foldable                  ( traverse_
                                                , for_
                                                )
import           Data.Ix                        ( range )
import           Control.Monad.ST
import           Data.Array.ST
import           Data.Array.IArray              ( (!)
                                                , (//)
                                                )
import qualified Data.Array.MArray             as MA
import qualified Data.Array.Unboxed            as UA
import qualified Data.Array.IArray             as IA

type Z2 = (Int, Int)
type Board = UA.UArray Z2 Bool
type MutableBoard s = STUArray s Z2 Bool

infile :: String
--infile = "input/day18.txt"
infile = "input/day18-test.txt"

n :: Int
n = 1

day18 :: IO ()
day18 = readFile infile >>= \contents ->
    print (part1 . parse $ contents) >> print (part2 . parse $ contents)

parse :: String -> Board
parse = toBoard . fmap toBinary . words

part1 :: Board -> Int
part1 = count . run 

part2 :: Board -> Int
part2 = count . runSet

dims :: [[a]] -> (Z2, Z2)
dims xss@(xs : _) = ((0, 0), (length xs - 1, length xss - 1))

toBinary :: String -> [Bool]
toBinary = fmap toBinaryC

toBinaryC :: Char -> Bool
toBinaryC '.' = False
toBinaryC '#' = True
toBinaryC _   = error "Unknown character"

toBoard :: [[Bool]] -> Board
toBoard bss = IA.listArray (dims bss) (concat bss)

count :: Board -> Int
count = length . filter id . IA.elems

run :: Board -> Board
run board = iterate step board !! n

runSet :: Board -> Board
runSet board = iterate stepSet board !! n

stepSet :: Board -> Board
stepSet arr = runSTUArray $ MA.thaw arr >>= \mutarr ->
    setCornersMut mutarr
        >> traverse_ (stepR arr mutarr) indices
        >> setCornersMut mutarr
        >> return mutarr
    where indices = range (IA.bounds arr)

step :: Board -> Board
step arr = runSTUArray $ MA.thaw arr >>= \mutarr ->
    traverse_ (stepR arr mutarr) indices >> return mutarr
    where indices = range (IA.bounds arr)

stepR :: Board -> MutableBoard s -> Z2 -> ST s ()
stepR arr mutarr r@(x, y) = 
    MA.writeArray mutarr r (runState (arr ! r) neighbors)
  where
    ((xmin, ymin), (xmax, ymax)) = IA.bounds arr
    neighbors = (arr !) <$> neighborR
    neighborR =
        [ (x + dx, y + dy)
        | dx <- [-1 .. 1]
        , dy <- [-1 .. 1]
        , abs dx + abs dy /= 0
        , x + dx >= xmin
        , y + dy >= ymin
        , x + dx <= xmax
        , y + dy <= ymax
        ]

runState :: Bool -> [Bool] -> Bool
runState v neighbors | v && (n == 2 || n == 3) = True
                     | v                       = False
                     | not v && n == 3         = True
                     | not v                   = False
    where n = length . filter id $ neighbors

setCornersMut :: MutableBoard s -> ST s ()
setCornersMut mutarr =
    MA.getBounds mutarr >>= \((xmin, ymin), (xmax, ymax)) ->
        MA.writeArray mutarr (xmin, ymin) True
            >> MA.writeArray mutarr (xmin, ymax) True
            >> MA.writeArray mutarr (xmax, ymin) True
            >> MA.writeArray mutarr (xmax, ymax) True
