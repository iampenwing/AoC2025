-- AdventOfCode 2025 Day 1, Puzzle 1
-- Secret Entrance
-- https://adventofcode.com/2025/day/1
-- How do I get in

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk

-- import File
import Data.String
import System.Environment
import Data.List.Split as Splitter
import AoCLib.AoCLib as AoC

--

--turnDial :: Int -> Int -> Int
--turnDial start distance = (mod (start + distance) 99)

runInstructions :: Int -> Int -> [Int] -> Int
runInstructions _ count [] = count
runInstructions start count (instruction:instructions) =
  runInstructions newStart (if (newStart == 0) then (count + 1) else count) instructions
  where
    newStart = (mod (start + instruction) 100)

readInstruction :: String -> Int
readInstruction ('L':distance) = 0 - (myReadInt distance)
readInstruction ('R':distance) = (myReadInt distance)

main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let answer = runInstructions 50 0 (map readInstruction (lines fileContents))
         in putStrLn (show answer)
