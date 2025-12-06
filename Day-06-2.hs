-- AdventOfCode 2025 Day 6, Puzzle 2
-- MathsHomework
-- https://adventofcode.com/2025/day/6
-- How do I get in

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk

-- import File
import Data.String
import System.Environment
import Data.List.Split as Splitter
import AoCLib.AoCLib as AoC

parseInput :: [String] -> ([[Int]], [Char])
parseInput s = ((iGetInts (AoC.rotateGrid (take (l-1) s))), (getOps (head (drop (l-1) s))))
  where
    l = length s

iGetInts :: [String] -> [[Int]]
iGetInts []     = []
iGetInts s = i:(iGetInts ss)
  where (i, ss) = iiGetInts s []

iiGetInts :: [String] -> [Int] -> ([Int], [String])
iiGetInts [] nums = (nums, [])
--iiGetInts ("":ss) nums = ((reverse nums), ss)
iiGetInts (s:ss) nums
  | [x | x <- s, (not (x `elem` " "))] == "" = (nums, ss)
  | otherwise = iiGetInts ss ((AoC.myReadInt s):nums)

getOps :: String -> [Char]
getOps s = [x | x <- s, not (x `elem` " " )]

doMaths :: [[Int]] -> [Char] -> [Int]
doMaths _ [] = []
doMaths (n:nums) (op:ops)
  | op == '+' = (foldr1 (+) n):(doMaths nums ops)
  | op == '*' = (foldr1 (*) n):(doMaths nums ops)

main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let (nums, ops) = parseInput (lines fileContents)
        in putStrLn (show (foldr1 (+) (doMaths nums ops)))

