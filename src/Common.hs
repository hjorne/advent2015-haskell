module Common (
  split
) where

isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (p:ps) (c:cs) = p == c && isPrefix ps cs

takeListWhile :: Eq a => [a] -> [a] -> ([a], [a])
takeListWhile _ [] = ([], [])
takeListWhile s xs@(x:xs') 
  | isPrefix s xs  = ([], drop n xs)
  | otherwise      = (x : match, rest)
  where n = length s
        (match, rest) = takeListWhile s xs'

split :: Eq a => [a] -> [a] -> [[a]]
split s xs | null rest = [match]
           | otherwise = match : split s rest
  where n = length s
        (match, rest) = takeListWhile s xs
