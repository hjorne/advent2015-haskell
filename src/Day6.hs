module Day6 where

import Control.Applicative
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Ix
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

explodeOperations :: [Operation] -> [Executable]
explodeOperations = foldr go []
    where go op acc = 
            explodeOperation op ++ acc
                    
explodeOperation :: Operation -> [Executable]
explodeOperation (Toggle r1 r2) = fmap (\pos -> Executable pos ((-) 1)) $ range (r1, r2)
explodeOperation (On r1 r2) = fmap (\pos -> Executable pos (const 1)) $ range (r1, r2)
explodeOperation (Off r1 r2) = fmap (\pos -> Executable pos (const 0)) $ range (r1, r2)

-- generateArray :: [Operation] -> UArray Location Int
-- generateArray ops = A.runSTUArray $ do
--     arr <- MA.newArray ((0, 0), (bound, bound)) 0
--     foldr go (pure ()) $ explodeOperations ops
--     fmap 
--     pure arr
--     where go exe _ = modifyArray arr exe

modifyArray :: MArray s -> Executable -> ST s ()
modifyArray arr (Executable r f) = do
    el <- MA.readArray arr r
    MA.writeArray arr r (f el)

tupleParse :: Parser Location
tupleParse = do
    skipMany (noneOf ['1'..'9'])
    x <- fromIntegral <$> integer
    char ','
    y <- fromIntegral <$> integer
    pure $ (x, y)

parse :: Parser [Operation]
parse = some $ token $ do
    op <- string "toggle" <|> string "turn off" <|> string "turn on"
    r1 <- tupleParse
    r2 <- tupleParse
    pure $ case op of
              "toggle" -> Toggle r1 r2
              "turn off" -> Off r1 r2
              "turn on" -> On r1 r2

input :: IO (Maybe [Operation])
input = parseFromFile parse "input/day6.txt" 

day6 :: IO ()
day6 = input >>= print
