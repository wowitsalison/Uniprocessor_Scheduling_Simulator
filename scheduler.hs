import System.IO (readFile)
import Data.List (words)
import Control.Monad (forM_)

-- Record to hold each input arrival time, priority, and cpu time
data Input = Input
    { arrival  :: Int
    , priority :: Int
    , cpu_time :: Int
    } deriving (Show)

-- Puts each string of arrival, priority, and cpu time into record
parseInput :: String -> Input
parseInput line = 
    let [a, p, c] = map read (words line)
    in Input a p c

main :: IO ()
main = do
    contents <- readFile "input.txt" -- placeholder filename
    let inputs = map parseInput (lines contents)
    forM_ inputs print