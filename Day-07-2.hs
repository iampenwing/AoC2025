-- AdventOfCode 2025 Day 7, Puzzle 2
-- Laboratories
-- https://adventofcode.com/2025/day/7
-- Tachyon Ray Splitting

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk

-- import File
import Data.String
import System.Environment
import Data.List.Split as Splitter
import AoCLib.AoCLib as AoC
--import Data.Set as Set

firstBeam :: String -> Int -> Int
firstBeam [] i = i
firstBeam (s:ss) i 
  | s == 'S'  = i
  | otherwise = firstBeam ss (i+1)

newProcessDiagram :: [String] -> [[Int]] -> [[Int]]
newProcessDiagram [] m = m
newProcessDiagram (l:ls) m = newProcessDiagram ls ((newProcessLine l (head m) 0):m)

newProcessLine :: String -> [Int] -> Int -> [Int]
newProcessLine [] _ _ = []
newProcessLine (l:ls) m n
  | l=='^'    = ((m!!(n-1)) + (m!!(n+1))):(newProcessLine ls m (n+1))
  | otherwise = (m!!n):(newProcessLine ls m (n+1))

newProcessLastLine :: String -> [Int]
newProcessLastLine [] = []
newProcessLastLine (l:ls)
  | (l=='^')  = 2:(newProcessLastLine ls)
  | otherwise = 1:(newProcessLastLine ls)

main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let answer = lines fileContents
         in putStrLn (show ((head ((newProcessDiagram (reverse (tail answer)) ((newProcessLastLine (head (reverse answer))):[]))))!!(firstBeam (head answer) 0)))
