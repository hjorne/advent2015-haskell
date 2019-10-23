{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import Data.Digest.Pure.MD5
import Data.ByteString

day4 :: IO ()
day4 = print "Day 4: " >> part1 >> part2

part1 :: IO ()
part1 = print "part1"

part2 :: IO ()
part2 = print "part2"

test = show $ md5 "hi"

