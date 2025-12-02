-- AdventOfCode 2025 Day 2, Puzzle 2
-- Gift Shop
-- https://adventofcode.com/2025/day/2
-- More Invalid IDs

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk

-- import File
import Data.String
--import Data.NumberLength
import System.Environment
import Data.List.Split as Splitter
import AoCLib.AoCLib as AoC


makeRange :: String -> (String, String)
makeRange s = (start, end)
  where (start:(end:_)) = Splitter.splitOn "-" s

checkRange :: String -> [Int]
checkRange s = iCheckRange (start, end) []
  where (start, end) = makeRange s

iCheckRange :: (String, String) -> [Int] -> [Int]
iCheckRange (s, e) l
  | (AoC.myReadInt s) > (AoC.myReadInt e) = l
  | otherwise                             = iCheckRange ((show ((AoC.myReadInt s)+1)), e) (if (checkNumber s 1) then ((AoC.myReadInt s):l) else l)

checkNumber :: String -> Int -> Bool
checkNumber s c
  | c > (div (length s) 2) = False
  | otherwise              = if (iCheckNumber chunks) then True else (checkNumber s (c+1))
    where chunks   = Splitter.chunksOf c s

iCheckNumber :: [String] -> Bool
iCheckNumber [] = False
iCheckNumber (i:[]) = False
iCheckNumber (i:(j:[])) = (i == j)
iCheckNumber (i:(j:k))
  | i == j    = iCheckNumber (j:k)
  | otherwise = False


main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let answer = foldr1 (+) (foldr1 (++) (map checkRange (Splitter.splitOn "," fileContents)))
         in putStrLn (show answer)
