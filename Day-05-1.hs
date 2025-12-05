-- AdventOfCode 2025 Day 5, Puzzle 1
-- Cafeteria
-- https://adventofcode.com/2025/day/5
-- What are the fresh ingredients?

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk

-- import File
import Data.String
import System.Environment
import Data.List.Split as Splitter
import AoCLib.AoCLib as AoC
-- import Data.Set as Set

parseInput :: [String] -> ([(Int, Int)], [Int])
parseInput (l:ls)
  | l == ""   = ([], (map (AoC.myReadInt) ls))
  | otherwise = (((makeRange l):s), is)
  where
    (s, is) = parseInput ls

makeRange :: String -> (Int, Int)
makeRange l = ((AoC.myReadInt s),(AoC.myReadInt e))
  where
    (s:(e:_)) = Splitter.splitOn "-" l

isFreshIngredient :: [(Int, Int)] -> Int -> Int
isFreshIngredient [] _ = 0
isFreshIngredient ((s, e):rs) i
  | i >= s && i <= e = 1
  | otherwise        = isFreshIngredient rs i

main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let (good, ingredients) = parseInput (lines fileContents)
         in putStrLn (show (foldr1 (+) (map (isFreshIngredient good) ingredients)))
