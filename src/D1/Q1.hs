module D1.Q1 (solve) where

import Data.List (sort, transpose)

parseCSV :: String -> [[Int]]
parseCSV contents =
  let readInt s = read s :: Int
      parseRow row = case words row of
        [a, b] -> map readInt [a, b]
        _ -> []
   in filter (not . null) $ map parseRow $ lines contents

solve' :: [[Int]] -> Int
solve' m = case transpose m of
  [as, bs] -> sum $ zipWith (\x y -> abs $ x - y) (sort as) (sort bs)
  _ -> 0

solve :: IO ()
solve = print . solve' . parseCSV =<< readFile "./src/D1/input.txt"
