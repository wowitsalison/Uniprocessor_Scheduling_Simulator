import System.IO (readFile)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Char (chr)
import Control.Monad (when)
import Control.Concurrent (threadDelay)

-- Record to hold each process input
data Input = Input
    { charId   :: Char,
      arrival  :: Int,
      priority :: Int,
      cpuTime  :: Int
    } deriving (Show, Eq)

-- Puts each string of arrival, priority, and cpu time into record
parseInput :: Int -> String -> Input
parseInputs lines = zipWith parseInput [0..] (tail lines)
parseInput index line = 
    let [a, p, c] = map read (words line)
    in Input (chr (index + 65)) a p c

-- Find the input with the lowest priority in the ready queue
findLowestPriority :: [Input] -> Maybe Input
findLowestPriority [] = Nothing
findLowestPriority queue = Just (minimumBy (comparing priority) queue)

-- Run one second of the scheduler
scheduler :: Int -> [Input] -> [Input] -> IO ()
scheduler seconds inputs readyQueue = do
    -- Add to ready queue
    let (newArrivals, remainingInputs) = span (\p -> arrival p == seconds) inputs
    let updatedQueue = readyQueue ++ newArrivals
        
    case findLowestPriority updatedQueue of
        Just process ->
            if cpuTime process > 0 then do
                putStrLn $ show seconds ++ "    " ++ [charId process]
                let updatedQueue' = updateQueue process updatedQueue
                threadDelay 1000000 -- Delay one second
                scheduler (seconds + 1) remainingInputs updatedQueue'
            else do
                let cleanedQueue = removeFromQueue process updatedQueue
                scheduler seconds remainingInputs cleanedQueue
        Nothing ->
            if null remainingInputs then putStrLn "END"
            else do
                print seconds
                threadDelay 1000000 -- Delay one second
                scheduler (seconds + 1) remainingInputs updatedQueue

-- Subtract 1 from cpuTime
updateQueue :: Input -> [Input] -> [Input]
updateQueue process queue =
    let updatedProcess = process { cpuTime = cpuTime process - 1}
    in replaceInQueue updatedProcess queue

-- Remove process from ready queue
removeFromQueue :: Input -> [Input] -> [Input]
removeFromQueue process = filter (\p -> charId p /= charId process)

replaceInQueue :: Input -> [Input] -> [Input]
replaceInQueue process queue
    | cpuTime process > 0 = withoutProcess ++ [process] -- Keep if still running
    | otherwise = withoutProcess                        -- Remove if finished
  where withoutProcess = removeFromQueue process queue
            
getQuanta :: IO Int
getQuanta = readLn

main :: IO ()
main = do
    putStrLn "Enter quanta for round robin: "
    quanta <- getQuanta
    contents <- readFile "input.txt" -- placeholder filename
    let allLines = lines contents
    let inputs = zipWith parseInput [0..] (tail allLines)  -- Skip the first line
    print quanta
    putStrLn "START"
    scheduler 0 inputs []