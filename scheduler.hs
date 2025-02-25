import System.IO (readFile)
import Data.List (words, minimumBy)
import Data.Ord (comparing)
import Data.Char (chr)
import Control.Monad (forM_, when)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)

-- Record to hold each process input
data Input = Input
    { charId   :: Char,
      arrival  :: Int,
      priority :: Int,
      cpuTime  :: Int
    } deriving (Show)

-- Puts each string of arrival, priority, and cpu time into record
parseInput :: Int -> String -> Input
parseInput index line = 
    let [a, p, c] = map read (words line)
    in Input (chr (index + 65)) a p c

-- Find the input with the lowest priority in the ready queue
findLowestPriority :: [Input] -> Maybe Input
findLowestPriority [] = Nothing
findLowestPriority queue = Just (minimumBy (comparing priority) queue)

-- Run one second of the scheduler
scheduler :: UTCTime -> [Input] -> [Input] -> IO ()
scheduler startTime inputs readyQueue = do
    currentTime <- getCurrentTime
    let seconds = floor (diffUTCTime currentTime startTime) :: Int
    let (x:xs) = inputs
    
    -- Add to ready queue
    let (newArrivals, remainingInputs) = span (\p -> arrival p == seconds) inputs
    let updatedQueue = readyQueue ++ newArrivals
        
    case findLowestPriority updatedQueue of
        Just process ->
            if cpuTime process > 0 then do
                putStrLn $ show seconds ++ [charId process]
                let updatedQueue = updateQueue process updatedQueue
                threadDelay 1000000 -- Delay one second
                scheduler startTime remainingInputs updatedQueue
            else
                scheduler startTime remainingInputs (removeFromQueue process updatedQueue)
        Nothing ->
            if null remainingInputs then putStrLn "END"
            else do
                print seconds
                threadDelay 1000000 -- Delay one second
                scheduler startTime remainingInputs updatedQueue

-- 
updateQueue :: Input -> [Input] -> [Input]
updateQueue process queue =
    let updatedProcess = process { cpuTime = cpuTime process - 1}
    in replaceInQueue updatedProcess queue

-- 
removeFromQueue :: Input -> [Input] -> [Input]
removeFromQueue process = filter (\p -> charId p /= charId process)

replaceInQueue :: Input -> [Input] -> [Input]
replaceInQueue process queue =
    let withoutProcess = removeFromQueue process queue
    in withoutProcess ++ [process]
            
main :: IO ()
main = do
    contents <- readFile "input.txt" -- placeholder filename
    let inputs = zipWith parseInput [0..] (lines contents)
    startTime <- getCurrentTime
    putStrLn "START"
    scheduler startTime inputs []