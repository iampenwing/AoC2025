-- AdventOfCode 2025 Day 2, Puzzle 2
-- Gift Shop
-- https://adventofcode.com/2025/day/2
-- Invalid IDs

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
  | odd (length s)                        = iCheckRange ((show ((AoC.myReadInt s)+1)), e) l
  | otherwise                             = iCheckRange ((show ((AoC.myReadInt s)+1)), e) (if (s1 == s2) then ((AoC.myReadInt s):l) else l)
    where (s1:(s2:_)) = Splitter.chunksOf (div (length s) 2) s
    
main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let answer = foldr1 (+) (foldr1 (++) (map checkRange (Splitter.splitOn "," fileContents)))
         in putStrLn (show answer)
