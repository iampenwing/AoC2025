-- AdventOfCode 2025 Day 6, Puzzle 1
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

parseInput :: [String] -> ([[Integer]], [Char]) -> ([[Integer]], [Char])
parseInput [] acc = acc
parseInput (s:ss) (acc, ops)
  | l == 1    = ((reverse (map reverse acc)), (getOps s)) -- get operators
  | otherwise = parseInput ss (((AoC.getInts s):acc), ops)
  where
    l = length (s:ss)

getOps :: String -> [Char]
getOps s = [x | x <- s, not (x `elem` " " )]

doMaths :: [[Integer]] -> [Char] -> [Integer]
doMaths _ [] = []
doMaths nums (op:ops)
  | op == '+' = (foldr1 (+) (map head nums)):(doMaths (map tail nums) ops)
  | op == '*' = (foldr1 (*) (map head nums)):(doMaths (map tail nums) ops)

main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let (nums, ops) = parseInput (lines fileContents) ([], [])
        in putStrLn (show (foldr1 (+) (doMaths nums ops)))

