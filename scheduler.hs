import System.IO (readFile)
import Data.List (words)
import Data.Char (chr)
import Control.Monad (forM_, when)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- Record to hold each input arrival time, priority, and cpu time
data Input = Input
    { id       :: Char,
      arrival  :: Int,
      priority :: Int,
      cpu_time :: Int
    } deriving (Show)

readyQueue :: [Int] 
readyQueue = []

-- Puts each string of arrival, priority, and cpu time into record
parseInput :: String -> Input
parseInput line = 
    let [a, p, c] = map read (words line)
        id = chr (index + 65)
    in Input id a p c

-- Remove head from queue
removeFirstInQueue :: [a] -> [a]
removeFirstInQueue [] = []
removeFirstInQueue (x:xs) = xs

-- Remove head from queue then replace it in the back
replaceInQueue :: [a] -> [a]
replaceInQueue [] = []
replaceInQueue (x:xs) = xs ++ [x]

-- Find the input with the lowest priority in the ready queue
findLowestPriority :: [Input] -> Input
findLowestPriority queue = minimumBy (comparing priority) queue

-- Check to see if the timer matches a value (arrival time)
checkTimer :: Int -> Int -> Bool
checkTimer x timer = x == timer

startTimer :: IO ()
startTimer = do
    startTime <- getCurrentTime
    putStrLn "START"

scheduler :: [Input] -> [Input] -> Int -> IO ()
scheduler startTime inputs readyQueue = do
    if null readyQueue && null inputs then
        putStrLn "END"
    else do
        currentTime <- getCurrentTime
        let seconds = floor (diffUTCTime currentTime startTime) :: Int
        let (x:xs) = inputs
        
        -- Check if the arrival time matches the timer's time
        if arrival x == seconds then do
            -- Add to ready queue and remove from 
            let updatedReadyQueue = readyQueue ++ [x]
            let updatedInputs = xs
            
            -- Check priority in the ready queue
            let lowestPriority = findLowestPriority readyQueue
            if cpu_time > 0 then
                print seconds
                print id lowestPriority
                cpu_time lowestPriority = cpu_time lowestPriority - 1
            else do
                -- Remove lowestPriority from readyQueue

            -- Round robin if equal priority

            print updatedReadyQueue
            scheduler startTime updatedInputs updatedReadyQueue
        else 
            print seconds
            threadDelay 1000000  -- 1 second delay
            scheduler startTime inputs readyQueue
            
main :: IO ()
main = do
    contents <- readFile "input.txt" -- placeholder filename
    let inputs = zipWith parseInput [0..] (lines contents)
    startTime = startTimer
    scheduler startTime inputs readyQueue