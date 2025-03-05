import System.IO (readFile)
import Data.List (minimumBy, partition)
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
scheduler :: Int -> Int -> [Input] -> [Input] -> Int -> IO ()
scheduler elapsedTime seconds inputs readyQueue quanta = do
    -- Add new arrivals to the ready queue
    let (newArrivals, remainingInputs) = span (\p -> arrival p == seconds) inputs
    let updatedQueue = readyQueue ++ newArrivals
    -- Run by lowest priority
    case findLowestPriority updatedQueue of
        Just process -> 
            let minPriorityValue = priority process
                (samePriority, others) = partition (\p -> priority p == minPriorityValue) updatedQueue
            in 
            if length samePriority > 1 then 
                let (current:rest) = samePriority
                in if cpuTime current > 0 then
                    roundRobin elapsedTime seconds remainingInputs samePriority others quanta
                else do
                    let cleanedQueue = removeFromQueue current (others ++ rest)
                    scheduler elapsedTime seconds remainingInputs cleanedQueue quanta
            else if cpuTime process > 0 then do
                putStrLn $ show seconds ++ "    " ++ [charId process]
                let updatedQueue' = updateQueue process updatedQueue
                -- Delay one second
                threadDelay 1000000 
                scheduler (elapsedTime + 1) (seconds + 1) remainingInputs updatedQueue' quanta
            else do
                let cleanedQueue = removeFromQueue process updatedQueue
                scheduler elapsedTime seconds remainingInputs cleanedQueue quanta
        Nothing ->
            if null remainingInputs then putStrLn "END"
            else do
                print seconds
                threadDelay 1000000
                scheduler elapsedTime (seconds + 1) remainingInputs updatedQueue quanta

-- Run processes with equal priorities for quanta seconds before switching
roundRobin :: Int -> Int -> [Input] -> [Input] -> [Input] -> Int -> IO ()
roundRobin elapsedTime seconds remainingInputs (current:rest) others quanta = do
    putStrLn $ show seconds ++ "    " ++ [charId current]
    let updatedProcess = current { cpuTime = cpuTime current - 1 }
        newElapsedTime = elapsedTime + 1
        updatedQueue' = if cpuTime updatedProcess > 0
                        -- Move to back only when switching
                        then rest ++ [updatedProcess]
                        -- Remove if finished   
                        else rest                       
    threadDelay 1000000
    -- Switch process if quanta is reached or process finished
    if newElapsedTime `mod` quanta == 0 || cpuTime updatedProcess == 0
        then scheduler newElapsedTime (seconds + 1) remainingInputs (others ++ updatedQueue') quanta
        else roundRobin newElapsedTime (seconds + 1) remainingInputs (updatedProcess : rest) others quanta

-- Subtract 1 from cpuTime
updateQueue :: Input -> [Input] -> [Input]
updateQueue process queue =
    let updatedProcess = process { cpuTime = cpuTime process - 1}
    in replaceInQueue updatedProcess queue

-- Remove process from ready queue
removeFromQueue :: Input -> [Input] -> [Input]
removeFromQueue process = filter (\p -> charId p /= charId process)

-- Add process to back of queue or remove
replaceInQueue :: Input -> [Input] -> [Input]
replaceInQueue process queue
    -- Keep if still running
    | cpuTime process > 0 = withoutProcess ++ [process] 
    -- Remove if finished
    | otherwise = withoutProcess                        
  where withoutProcess = removeFromQueue process queue

-- Get integer from user input for quanta
getQuanta :: IO Int
getQuanta = readLn

main :: IO ()
main = do
    putStrLn "Please enter the path to the input file with no quotation marks:"
    filePath <- getLine
    putStrLn "Enter quanta for round robin: "
    quanta <- getQuanta
    contents <- readFile filePath
    let allLines = lines contents
    let inputs = zipWith parseInput [0..] (tail allLines)
    putStrLn "START"
    scheduler 0 0 inputs [] quanta