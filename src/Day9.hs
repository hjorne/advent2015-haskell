module Day9 where

import Text.Trifecta

data Edge = Edge ID ID Integer deriving (Eq, Show)
type ID = String

infile :: String
infile = "input/day9.txt"

day9 :: IO ()
day9 = parseFromFile (some parseLine) infile >>= print

parseLine :: Parser Edge
parseLine = mkEdge <$> parseID <*> symbol "to" <*> parseID <*> symbolic '=' <*> natural
    where mkEdge id1 _ id2 _ = Edge id1 id2
          parseID = token $ some letter
