module Day8 where

import Text.Trifecta
import Control.Applicative
import Data.Functor


escQuote :: Parser Int
escQuote = string "\\\"" $> 1

escSlash :: Parser Int
escSlash = string "\\\\" $> 2

hex :: Parser Int
hex = try $ char '\\' *> char 'x' *> hexDigit *> hexDigit $> 1

specials :: Parser Int
specials = escQuote <|> escSlash <|> hex

charT :: Parser Int
charT = anyChar $> 1

strtokens :: Parser [Int]
strtokens = some $ specials <|> charT 

str :: Parser [Int]
str = char '"' *> strtokens <* notFollowedBy (char '"' *> eof)

day8 :: IO ()
day8 = do
    let result = sum <$> parseString str mempty "\"a\""
    print result