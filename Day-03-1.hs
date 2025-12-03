-- AdventOfCode 2025 Day 3, Puzzle 1
-- Lobby
-- https://adventofcode.com/2025/day/3
-- Power Banks

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk

-- import File
import Data.String
import System.Environment
import Data.List.Split as Splitter
import AoCLib.AoCLib as AoC

makeInt :: (Int, Int) -> Int
makeInt (i, j) = (i * 10) + j

bestPower :: (Int, Int) -> String -> Int
bestPower best [] = makeInt best
bestPower (i, j) (d:[]) = makeInt (if (nd>j) then (i, nd) else (i, j))
  where nd = case d of
          '0' -> 0
          '1' -> 1
          '2' -> 2
          '3' -> 3
          '4' -> 4
          '5' -> 5
          '6' -> 6
          '7' -> 7
          '8' -> 8
          '9' -> 9
          
bestPower (i, j) (d:digits) 
  | i==0 = bestPower (nd, j) digits 
  | i/=0 && nd > i = bestPower (nd, 0) digits 
  | i/=0 && nd > j = bestPower (i, nd) digits 
  | otherwise = bestPower (i, j) digits
  where nd = case d of
          '0' -> 0
          '1' -> 1
          '2' -> 2
          '3' -> 3
          '4' -> 4
          '5' -> 5
          '6' -> 6
          '7' -> 7
          '8' -> 8
          '9' -> 9

main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let answer = foldr1 (+) (map (bestPower (0,0)) (lines fileContents))
         in putStrLn (show answer)
