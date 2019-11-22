{-# LANGUAGE ViewPatterns #-}

module Day14 where

import qualified Data.Heap as H

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

data DeerState = DeerState {
    stateDeer :: Deer,
    statePosition :: Distance,
    state :: State
} deriving (Eq, Show)

data State = Resting 
           | Moving
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
    print $ part2 file

part1 :: String -> Int
part1 = getMaxEvent . finish . iterate step . initSim . parseDeers

finish :: [Simulation] -> Simulation
finish =  head . dropWhile (not . checkFinished)

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

-- part2 :: String -> Simulation
part2 s = length $ fst $ span (\e -> eventTime e == 0) $ allEvents deers
    where deers = parseDeers s

stepSecond :: Time -> [DeerState] -> [Event] -> 

allEvents :: [Deer] -> [Event]
allEvents = fmap H.minimum . takeWhile (not . checkFinished) . iterate step . initSim

initDeerState :: [Deer] -> [DeerState]
initDeerState = fmap mkState
    where mkState deer = DeerState deer 0 Moving

moveState :: DeerState -> DeerState
moveState dstate@(DeerState _ _ Resting) = dstate
moveState (DeerState deer pos Moving) = DeerState deer (pos + rate) Moving
    where rate = deerRate deer