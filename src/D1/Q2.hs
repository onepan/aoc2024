module D1.Q2 (solve) where

import Data.List (group, sort, transpose)
import qualified Data.Map.Strict as Map

parseCSV :: String -> [[Int]]
parseCSV contents =
  let readInt s = read s :: Int
      parseRow row = case words row of
        [a, b] -> map readInt [a, b]
        _ -> []
   in filter (not . null) $ map parseRow $ lines contents

solve' :: [[Int]] -> Int
solve' [as, bs] =
  let freq = Map.fromAscList $ map (\x -> (head x, length x)) $ group $ sort bs
      score x = case num of
        Just n -> head x * n * length x
        Nothing -> 0
        where
          num = Map.lookup (head x) freq
   in sum $ map score $ group $ sort as
solve' _ = 0

solve :: IO ()
solve = print . solve' . transpose . parseCSV =<< readFile "./src/D1/input.txt"
