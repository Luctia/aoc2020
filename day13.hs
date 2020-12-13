import Data.List.Split

getTimestamps :: [String] -> [Int]
getTimestamps [] = []
getTimestamps (i:is)
  | i == "x" = getTimestamps is
  | otherwise = (read i::Int):getTimestamps is

getBestBus :: Int -> [Int] -> Int -> Int
getBestBus _ [] currentBest = currentBest
getBestBus start (i:is) currentBest
  | (((div start i) + 1) * i) - start < (((div start currentBest) + 1) * currentBest) - start = getBestBus start is i
  | otherwise = getBestBus start is currentBest

getResult :: Int -> Int -> Int
getResult bus start = ((((div start bus) + 1) * bus) - start) * bus

part1' :: [[String]] -> Int
part1' [[start], is] = getResult (getBestBus startInt (getTimestamps is) 10000) startInt
                     where startInt = (read start::Int)

part1 = (part1' . map (splitOn ",") . lines) <$> readFile "day13data.txt"