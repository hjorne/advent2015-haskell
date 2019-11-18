{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Day14 where

import qualified Data.Heap as H
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (maximumBy)

type Name = String
type Rate = Int
type Distance = Int
type Time = Int
type Points = Int
type Simulation = H.Heap Event

data Deer = Deer {
    deerName :: Name,
    deerRate :: Rate,
    deerStamina :: Time,
    deerRest :: Time
} deriving (Eq, Show, Ord)

data Event = Event {
    eventTime :: Time,
    eventPosition :: Distance,
    eventDeer :: Deer,
    eventType :: EventType
} deriving (Eq, Show)

data EventType = StartMove
               | EndMove
               deriving (Eq, Show)
             
instance Ord Event where
    (Event t1 _ _ _) <= (Event t2 _ _ _) = t1 <= t2

infile :: String
infile = "input/day14.txt"

endTime :: Int
endTime = 2503

day14 :: IO ()
day14 = do 
    file <- readFile infile 
    print $ part1 file

part1 :: String -> Int
part1 = getMaxEvent . finish . iterate step . initSim . parseDeers

finish :: [Simulation] -> Simulation
finish =  head . dropWhile (not . checkFinished)

calculateWinner :: Simulation -> Map Deer Points -> Map Deer Points
calculateWinner sim = M.adjust (+1) leadDeer 
    where nextEventTime = eventTime $ H.minimum $ step sim
          leadDeer = getLeadDeer sim

stepSecond :: Simulation -> Simulation
stepSecond sim = undefined

moveSecond :: Event -> Event
moveSecond (Event t r deer EndMove) = Event (t + 1)
moveSecond (Event t r deer StartMove) = Event (t + 1) r deer StartMove
          
getLeadDeer :: Simulation -> Deer
getLeadDeer = eventDeer . maximumBy furthest . H.toUnsortedList
    where furthest e1 e2 = compare (eventPosition e1) (eventPosition e2)

getMaxEvent :: Simulation -> Int
getMaxEvent events = maximum $ eventPosition <$> H.toUnsortedList events

checkFinished :: Simulation -> Bool
checkFinished (H.minimum -> Event t _ _ _) = t == endTime

parseLine :: String -> Deer
parseLine (words -> [name,"can","fly", distance,
                     "km/s","for",len,"seconds,",
                     "but","then","must","rest",
                     "for",timeout,"seconds."]) = Deer name (read distance) (read len) (read timeout)
parseLine _ = error "Bad input"

parseDeers :: String -> [Deer]
parseDeers = fmap parseLine . lines

initSim :: [Deer] -> Simulation
initSim = H.fromList . fmap initDeer

initDeer :: Deer -> Event
initDeer deer = Event 0 0 deer StartMove

moveDeer :: Deer -> Time -> Distance -> (Time, Distance)
moveDeer (Deer _ rate len _) startT startD 
    | newT > endTime = (endTime, startD + rate * (endTime - startT))
    | otherwise = (newT, startD + rate * len)
    where newT = startT + len

move :: Event -> Event 
move (Event time pos deer StartMove) = 
    Event newTime newPos deer EndMove
    where (newTime, newPos) = moveDeer deer time pos
move (Event time pos deer EndMove) = 
    Event newTime pos deer StartMove
    where (Deer _ _ _ restTime) = deer
          naiveT = time  + restTime
          newTime = if naiveT < endTime 
                    then naiveT else endTime

step :: Simulation -> Simulation
step (H.uncons -> Just (event, sim)) = H.insert newEvent sim
    where newEvent = move event
step _ = error "TODO: This should be the end of the simulation"
