import System.IO (readFile)
import Data.List (words)
import Control.Monad (forM_, when)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- Record to hold each input arrival time, priority, and cpu time
data Input = Input
    { arrival  :: Int
    , priority :: Int
    , cpu_time :: Int
    } deriving (Show)

readyQueue :: [Int] 
readyQueue = []

-- Puts each string of arrival, priority, and cpu time into record
parseInput :: String -> Input
parseInput line = 
    let [a, p, c] = map read (words line)
    in Input a p c

-- Remove head from queue
removeFirstInQueue :: [a] -> [a]
removeFirstInQueue [] = []
removeFirstInQueue (x:xs) = xs

-- Remove head from queue then replace it in the back
replaceInQueue :: [a] -> [a]
replaceInQueue [] = []
replaceInQueue (x:xs) = xs ++ [x]

-- Check to see if the timer matches a value (arrival time)
checkTimer :: Int -> Int -> Bool
checkTimer x timer = x == timer

startTimer :: IO ()
startTimer = do
    startTime <- getCurrentTime
    putStrLn "START"

scheduler :: [Input] -> [Input] -> Int -> IO ()
scheduler startTime inputs readyQueue = do
    if null readyQueue then
        putStrLn "END"
    else do
        currentTime <- getCurrentTime
        let seconds = floor (diffUTCTime currentTime startTime) :: Int
        let (x:xs) = inputs
        
        let arrival = arrival x == seconds
        if arrival then do
            let updatedReadyQueue = readyQueue ++ [x]
            let updatedInputs = xs
            print seconds
            print updatedReadyQueue
            scheduler startTime updatedInputs updatedReadyQueue
        else 
            print seconds
            threadDelay 1000000  -- 1 second delay
            scheduler startTime inputs readyQueue
            
main :: IO ()
main = do
    contents <- readFile "input.txt" -- placeholder filename
    let inputs = map parseInput (lines contents)
    startTimer
    forM_ inputs print