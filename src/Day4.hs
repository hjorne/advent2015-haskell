{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Binary
import Data.Binary.Put

day4 :: IO ()
day4 = print "Day 4: " >> part1 >> part2

part1 :: IO ()
part1 = print $ getkey 5 "yzbqklnj"

part2 :: IO ()
part2 = print $ getkey 6 "yzbqklnj"

getkey :: Int -> String -> Int
getkey n key = fst . head $ dropWhile (\(i, hash) -> hash /= replicate n '0') hashes
    where hashes = fmap f [1..]
          f x = (x, take n $ show $ md5 $ B.pack $ key ++ show x)

