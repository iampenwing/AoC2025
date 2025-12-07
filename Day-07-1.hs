-- AdventOfCode 2025 Day 7, Puzzle 1
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
import Data.Set as Set

firstBeam :: String -> Int -> Set.Set Int
firstBeam [] i = Set.singleton i
firstBeam (s:ss) i 
  | s == 'S'  = Set.singleton i
  | otherwise = firstBeam ss (i+1)

processDiagram :: [String] -> Set.Set Int -> Int -> Int
processDiagram [] _ c = c
processDiagram (l:ls) tbs c = processDiagram ls nbs (c+nc)
  where
    (nbs, nc) = processLine tbs 0 l Set.empty
    

processLine :: Set.Set Int -> Int -> String -> Set.Set Int -> (Set.Set Int, Int)
processLine tbs c [] ntbs = (ntbs, c)
processLine tbs c ml ntbs
  | (Set.null tbs) = (ntbs, c)
  | otherwise  = processLine (Set.deleteAt 0 tbs) (c+nc) ml (Set.union nbs ntbs)
  where
    (nbs, nc) = iProcessLine ml (Set.elemAt 0 tbs)
    

iProcessLine :: String -> Int -> (Set.Set Int, Int)
iProcessLine s b
  | (b==0) && ((s!!b) == '^') = (Set.singleton (b+1), 1)
  | (b/=0) && ((s!!b) == '^') && (b < len) = (Set.insert (b-1) (Set.singleton (b+1)), 1)
  | (b/=0) && ((s!!b) == '^') && (b == len) = (Set.singleton (b-1), 1)
  | otherwise = (Set.singleton b, 0)
  where
    len = (length s)
    
main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let answer = lines fileContents
         in putStrLn (show (processDiagram (tail answer) (firstBeam (head answer) 0) 0))
