module Day6 where

import Data.Array.Unboxed
import qualified Data.Array.ST as A
import qualified Data.Array.MArray as MA

data Operation = 
      Toggle
    | On
    | Off
    deriving (Eq, Show)

data Location = 
     Location Int Int
     deriving (Eq, Show)

testArray :: Array (Int, Int) Int
testArray = array ((0, 0), (9, 9)) [((i, j), i * j) | i <- [0..9], j <- [0..9]]

testSTArray :: UArray Int Int
testSTArray = A.runSTUArray $ do
    let q = MA.newArray (0, 9) 0
    MA.writeArray q (5 :: Int) (10 :: Int)
    q