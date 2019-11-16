{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Day12 where

import Data.Maybe (fromJust)

import Text.Trifecta
import Data.Aeson
import Data.HashMap.Strict
import Data.Text
import Data.Monoid 
import Data.Scientific

infile :: String
infile = "input/day12.txt"

part1 :: IO ()
part1 = parseFromFile (some anyNum) infile >>= print . fmap sum

part2 :: IO ()
part2 = (decodeFileStrict infile :: IO (Maybe Value)) >>= print . toScientific . fromJust

day12 :: IO ()
day12 = part2

digitC :: String
digitC = ['1'..'9'] ++ ['-']

noDigits :: Parser String
noDigits = many $ noneOf digitC

anyNum :: Parser Integer
anyNum = integer `surroundedBy` noDigits

toScientific :: Value -> Scientific
toScientific (Number x) = x
toScientific (Array xs) = sum $ toScientific <$> xs
toScientific (Object obj) 
    | isObjectRed obj = 0
    | otherwise = sum $ toScientific <$> obj
toScientific _ = 0

isObjectRed :: Object -> Bool
isObjectRed = getAny . foldMap (Any . isRed)
    where isRed (String "red") = True
          isRed _ = False