-- AdventOfCode 2025 Day 1, Puzzle 1
-- Secret Entrance
-- https://adventofcode.com/2025/day/1
-- How do I get in

-- Alex Lambert (penwing)
-- aoc@penwing.me.uk

-- import File
import Data.String
import System.Environment
import Data.List.Split as Splitter
import AoCLib.AoCLib as AoC

--

countAround :: (Int, Int) -> [String] -> Char
countAround (n, m) s
  | m == 0 && n == 0             = if (item == '@') then 'x' else '.' -- top left (corners are always fewer then 4)
  | m == (y - 1) && n == 0       = if (item == '@') then 'x' else '.' -- bottom left
  | m == 0 && n == (x - 1)       = if (item == '@') then 'x' else '.' -- top right
  | m == (y - 1) && n == (x - 1) = if (item == '@') then 'x' else '.' -- bottom right
  | m == 0                       = if (item == '@') then 
                                     (if (((if (((s!!m)!!(n-1)) == '@') then 1 else 0) +
                                           (if (((s!!m)!!(n+1)) == '@') then 1 else 0) +
                                           (if (((s!!(m+1))!!n) == '@') then 1 else 0) +
                                           (if (((s!!(m+1))!!(n-1)) == '@') then 1 else 0) +
                                           (if (((s!!(m+1))!!(n+1)) == '@') then 1 else 0)) < 4) then 'x' else '@') else '.' -- top row, not a corner
  | m == (y - 1)                 = if (item == '@') then
                                     (if (((if (((s!!m)!!(n-1)) == '@') then 1 else 0) +
                                           (if (((s!!m)!!(n+1)) == '@') then 1 else 0) +
                                           (if (((s!!(m-1))!!n) == '@') then 1 else 0) +
                                           (if (((s!!(m-1))!!(n-1)) == '@') then 1 else 0) +
                                           (if (((s!!(m-1))!!(n+1)) == '@') then 1 else 0)) < 4) then 'x' else '@') else '.' -- bottom row, not a corner
  | n == 0                       = if (item == '@') then
                                     (if (((if (((s!!(m-1))!!n) == '@') then 1 else 0) +
                                           (if (((s!!(m+1))!!n) == '@') then 1 else 0) +
                                           (if (((s!!m)!!(n+1)) == '@') then 1 else 0) +
                                           (if (((s!!(m-1))!!(n+1)) == '@') then 1 else 0) +
                                           (if (((s!!(m+1))!!(n+1)) == '@') then 1 else 0)) < 4) then 'x' else '@') else '.' -- left column, not a corner
  | n == (x - 1)                 = if (item == '@') then
                                     (if (((if (((s!!(m-1))!!n) == '@') then 1 else 0) +
                                           (if (((s!!(m+1))!!n) == '@') then 1 else 0) +
                                           (if (((s!!m)!!(n-1)) == '@') then 1 else 0) +
                                           (if (((s!!(m-1))!!(n-1)) == '@') then 1 else 0) +
                                           (if (((s!!(m+1))!!(n-1)) == '@') then 1 else 0)) < 4) then 'x' else '@') else '.' -- right column, not a corner
  | otherwise                    = if (item == '@') then
                                     (if (((if (((s!!(m-1))!!n) == '@') then 1 else 0) +
                                           (if (((s!!(m+1))!!n) == '@') then 1 else 0) +
                                           (if (((s!!m)!!(n+1)) == '@') then 1 else 0) +
                                           (if (((s!!(m-1))!!(n+1)) == '@') then 1 else 0) +
                                           (if (((s!!m)!!(n-1)) == '@') then 1 else 0) +
                                           (if (((s!!(m-1))!!(n-1)) == '@') then 1 else 0) +
                                           (if (((s!!(m+1))!!(n-1)) == '@') then 1 else 0) +
                                           (if (((s!!(m+1))!!(n+1)) == '@') then 1 else 0)) < 4) then 'x' else '@') else '.' -- middle
  where x = length (head s)
        y = length s
        item = (s!!m)!!n

removeRolls :: Int -> [String] -> [String] -> [String]
removeRolls j o s
  | j == y    = reverse o
  | otherwise = removeRolls (j+1) ((removeRollsRow (0, j, (length (head s))) s):o) s
  where y = (length s)

removeRollsRow :: (Int, Int, Int) -> [String] -> String
removeRollsRow (i, j, x) s
  | i == x    = []
  | otherwise = ((countAround (i, j) s):(removeRollsRow ((i+1), j, x) s))

countRemovedRow :: String -> Int
countRemovedRow [] = 0
countRemovedRow (s:ss)
  | s == 'x'  = 1 + (countRemovedRow ss)
  | otherwise = 0 + (countRemovedRow ss)

removeRollsIter :: Int -> [String] -> Int
removeRollsIter c s = if (x == 0) then c else (removeRollsIter (c+x) news)
  where
    news = removeRolls 0 [] s
    x = foldr1 (+) (map (countRemovedRow) news)

findAccessible :: (Int, Int) -> [String] -> Int
findAccessible (i, j) s
  | j == y = 0
  | i < x  = (if ((countAround (i, j) s) == 'x') then 1 else 0) + (findAccessible ((i + 1), j) s)
  | i == x = findAccessible (0, (j + 1)) s
  where x = length (head s)
        y = length s

--iCountAround :: Int -> Int -> [String] -> [[Int]] -> [[Int]]
--iCountAround x y 


main :: IO()
main = do
  [fileInput] <- getArgs
  fileContents <- readFile fileInput
  let answer = removeRollsIter 0 (lines fileContents)
         in putStrLn (show answer)
