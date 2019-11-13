module Day8 (day8) where

import Text.Trifecta
import Control.Applicative
import Control.Monad
import Data.Functor

infile :: String
infile = "input/day8.txt"

escQuote :: Parser Int
escQuote = string "\\\"" $> 1

escSlash :: Parser Int
escSlash = string "\\\\" $> 1

hex :: Parser Int
hex = try $ char '\\' *> char 'x' *> hexDigit *> hexDigit $> 1

specials :: Parser Int
specials = hex <|> escSlash <|> escQuote 

charI :: Parser Int
charI = notChar '"' $> 1

strtokens :: Parser [Int]
strtokens = some $ specials <|> charI 

str :: Parser Int
str = fmap sum $ token $ strtokens `surroundedBy` char '"'

manyStr :: Parser Int
manyStr = sum <$> many str

encodeChar :: Char -> String
encodeChar '\\' = "\\\\"
encodeChar '"' = "\\\""
encodeChar x = [x]

encode :: String -> String
encode s = '"' : s' ++ "\""
    where s' = s >>= encodeChar

part1 :: IO ()
part1 = parseFromFile manyStr infile >>= 
    \properCount -> 
    sum . fmap length . lines <$> readFile infile >>= 
    \naiveCount -> 
    print $ (-) <$> pure naiveCount <*> properCount

part2 :: IO ()
part2 = 
    readFile infile >>=
    \f ->
    (pure . sum . fmap length . lines $ f) >>=
    \naiveCount -> 
    (pure . sum . fmap (length . encode) . lines $ f) >>=
    \encodeCount ->
    print $ encodeCount - naiveCount

day8 :: IO ()
day8 = part1 >> part2