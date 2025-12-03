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

toDigits :: String -> [Int]
toDigits "" = []
toDigits (d:ds) = (nd:(toDigits ds))
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

makeNumber :: Int -> [Int] -> Int
makeNumber n [] = n
makeNumber n (m:ms) = makeNumber ((n*10) + m) ms

newBestPower :: Int -> [Int] -> [Int] -> [Int]
newBestPower 0 p r = reverse p
newBestPower n p r = newBestPower (n - 1) (i:p) (drop j r)
  where (i, j) = iNewBestPower (take ((length r) - (n - 1)) r) (0, 0) 0

iNewBestPower :: [Int] -> (Int, Int) -> Int -> (Int, Int)
iNewBestPower [] r _ = r
iNewBestPower (d:ds) (m, n) l
  | d > m = iNewBestPower ds (d, (l+1)) (l+1)
  | otherwise = iNewBestPower ds (m, n) (l+1)



main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let answer = foldr1 (+) (map (makeNumber 0) (map (newBestPower 12 []) (map toDigits (lines fileContents))))
    in putStrLn (show answer)
