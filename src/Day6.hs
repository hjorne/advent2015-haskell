module Day6 where

import Control.Applicative
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Ix
import Data.Foldable (for_, traverse_)
import qualified Data.Array (elems)
import qualified Data.Array.ST as A
import qualified Data.Array.MArray as MA
import Text.Trifecta

data Operation = 
      Toggle Location Location
    | On Location Location
    | Off Location Location
    deriving (Eq, Show)

data Executable = Executable Location (Int -> Int)

type Location = (Int, Int)
type MArray s = A.STUArray s Location Int

bound :: Int
bound = 999


executeOperationP1 :: MArray s -> Operation -> ST s ()
executeOperationP1 arr (Toggle r1 r2) = execute arr (r1, r2) (1-)
executeOperationP1 arr (On r1 r2) = execute arr (r1, r2) (const 1)
executeOperationP1 arr (Off r1 r2) = execute arr (r1, r2) (const 0)

execute :: MArray s -> (Location, Location) -> (Int -> Int) -> ST s ()
execute arr (r1, r2) f = 
    for_ (range (r1, r2)) (\r -> MA.readArray arr r >>=
                           \x -> MA.writeArray arr r (f x))

executeOperationP2 :: MArray s -> Operation -> ST s ()
executeOperationP2 arr (Toggle r1 r2) = execute arr (r1, r2) (+2)
executeOperationP2 arr (On r1 r2) = execute arr (r1, r2) (+1)
executeOperationP2 arr (Off r1 r2) = execute arr (r1, r2) (\x -> max (x - 1) 0)

generateArray1 :: [Operation] -> UArray Location Int
generateArray1 ops = A.runSTUArray $ do
    arr <- MA.newArray ((0, 0), (bound, bound)) 0 
    traverse_ (executeOperationP1 arr) ops
    return arr

generateArray2 :: [Operation] -> UArray Location Int
generateArray2 ops = A.runSTUArray $ do
    arr <- MA.newArray ((0, 0), (bound, bound)) 0 
    traverse_ (executeOperationP2 arr) ops
    return arr

tupleParse :: Parser Location
tupleParse = do
    skipMany (noneOf ['1'..'9'])
    x <- fromIntegral <$> integer
    char ','
    y <- fromIntegral <$> integer
    pure (x, y)

parse :: Parser [Operation]
parse = some $ token $ do
    op <- string "toggle" <|> string "turn off" <|> string "turn on"
    r1 <- tupleParse
    r2 <- tupleParse
    pure $ case op of
              "toggle" -> Toggle r1 r2
              "turn off" -> Off r1 r2
              "turn on" -> On r1 r2

ioinput :: IO (Maybe [Operation])
ioinput = parseFromFile parse "input/day6.txt" 

part1 :: [Operation] -> Int
part1 ops = sum . elems $ generateArray1 ops

part2 :: [Operation] -> Int
part2 ops = sum . elems $ generateArray2 ops

day6 :: IO ()
day6 = do
    input <- ioinput
    -- print $ part1 <$> input
    print $ part2 <$> input
    return ()