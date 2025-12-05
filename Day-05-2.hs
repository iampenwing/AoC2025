-- AdventOfCode 2025 Day 5, Puzzle 2
-- Cafeteria
-- https://adventofcode.com/2025/day/1
-- What are the fresh ingredients

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk

-- import File
import Data.String
import System.Environment
import Data.List.Split as Splitter
import AoCLib.AoCLib as AoC
-- import Data.Set as Set

parseInput :: [String] -> [(Int, Int)]
parseInput (l:ls)
  | l == ""   = []
  | otherwise = ((makeRange l):s)
  where
    s = parseInput ls

makeRange :: String -> (Int, Int)
makeRange l = ((AoC.myReadInt s),(AoC.myReadInt e))
  where
    (s:(e:_)) = Splitter.splitOn "-" l

flattenRanges :: [(Int, Int)] -> [(Int, Int)]
flattenRanges [] = []
flattenRanges ((s, e):ranges) = (s, ne):(flattenRanges remainingRanges)
  where (ne, remainingRanges) = findNextRange (s, e) ranges

findNextRange :: (Int, Int) -> [(Int, Int)] -> (Int, [(Int, Int)])
findNextRange (s, e) [] = (e, [])
findNextRange (s, e) ((ns, ne):ranges)
  | e >= ne   = findNextRange (s, e) ranges
  | e >= ns   = findNextRange (s, ne) ranges
  | otherwise = (e, ((ns, ne):ranges))
  
sortRanges :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
sortRanges [] sorted = sorted
sortRanges ((s, e):rs) srs = (sortRanges rs (insertRange (s,e) srs))

insertRange :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
insertRange s [] = s:[]
insertRange (s, e) ((ss, se):srs)
  | s <= ss   = (s, e):((ss,se):srs)
  | otherwise = (ss,se):(insertRange (s, e) srs)

sizeOfRange :: (Int, Int) -> Int
sizeOfRange (s, e) = (e - s) + 1

main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let good = foldr1 (+) (map sizeOfRange (flattenRanges (sortRanges (parseInput (lines fileContents)) [])))
         in putStrLn (show (good))
