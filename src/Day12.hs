{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import Data.Maybe (fromJust)

import Text.Trifecta
import Data.Aeson
import Data.HashMap.Strict
import Data.Text

infile :: String
infile = "input/day12.txt"

part1 :: IO ()
part1 = parseFromFile (some anyNum) infile >>= print . fmap sum

part2 :: IO ()
part2 = (decodeFileStrict infile :: IO (Maybe Value)) >>= print

-- isRed :: [Value] -> Bool
-- isRed (Object o) = 
-- is


day12 :: IO ()
day12 = part2

digitC :: String
digitC = ['1'..'9'] ++ ['-']

noDigits :: Parser String
noDigits = many $ noneOf digitC

anyNum :: Parser Integer
anyNum = integer `surroundedBy` noDigits