import Data.List.Split
import Data.List

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

part1 = (part1' . map (splitOn ",") . lines) <$> readFile "day13input.txt"

-- For part 2, we use the Chinese remainder theorem.

getIdIndexPairs :: Int -> [String] -> [(Int, Int)]
getIdIndexPairs index xs
  | index > length xs - 1 = []
  | xs !! index == "x" = getIdIndexPairs (index + 1) xs
  | otherwise = (index, (read (xs !! index)::Int)):(getIdIndexPairs (index + 1) xs)

getSolution :: [(Int, Int)] -> Int
getSolution pairs = total `mod` bign
                  where bign = foldl (*) 1 (map snd pairs)
                        total = sum [ b * n * x | pair <- pairs,
                                                  let b = (snd pair) - (fst pair),
                                                  let n = div bign (snd pair),
                                                  let x = calculateX n (snd pair) ]

calculateX :: Int -> Int -> Int
calculateX multiplier modulator = head [ x | x <- [1..multiplier], mod (x * multiplier) modulator == 1 ]

part2 = (getSolution . getIdIndexPairs 0 . splitOn "," . (!! 1) .lines) <$> readFile "day13input.txt"