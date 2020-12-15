input :: [Int]
input = [16, 1, 0, 18, 12, 14, 19]
-- input = [3,1,2]

playGame :: [(Int, Int)] -> Int -> Int -> Int -> Int
playGame mentions previous i max
  | i == max = if previousMention == (-1) then 0 else distance
  | previousMention == (-1) = playGame ((previous, i - 1):mentions) 0 (i + 1) max
  | otherwise = playGame (addPrevious mentions previous (i - 1)) distance (i + 1) max
  where distance = i - 1 - previousMention
        previousMention = findPreviousMention mentions previous

addPrevious :: [(Int, Int)] -> Int -> Int -> [(Int, Int)]
addPrevious (mention:mentions) number i
  | fst mention == number = (number, i):mentions
  | otherwise = mention:(addPrevious mentions number i)

findPreviousMention :: [(Int, Int)] -> Int -> Int
-- findPreviousMention [] _ = (-1)
-- findPreviousMention (pair:pairs) needle
--   | fst pair == needle = snd pair
--   | otherwise = findPreviousMention pairs needle
findPreviousMention mentions needle
  | filtered == [] = (-1)
  | otherwise = snd $ head filtered
  where filtered = filter (\x -> fst x == needle) mentions

part1 = playGame (zip input [1..]) (last input) (length input + 1) 2020