import Data.List

numberIsValid :: [Int] -> Int -> Bool
numberIsValid list index
  | index < 5 = True
  | elem (list !! index) (allAditions subset) = True
  | otherwise = False
  where subset = take 5 . drop (index - 5) $ list

allAditions :: [Int] -> [Int]
allAditions list = [ x + y | x <- list, y <- list]

getFirstMistake :: Int -> [Int] -> Int
getFirstMistake index list
  | numberIsValid list index = getFirstMistake (index + 1) list
  | otherwise = list !! index

part1 = (getFirstMistake 5 . map (\x -> read x::Int) . lines) <$> readFile "day9input.txt"

incorrectNumber = 1492208709

findCombo :: Int -> [Int] -> [Int]
findCombo outcome list = head $ filter ((==outcome) . sum) (allContiguousSubsets 0 list)

findWeakness :: [Int] -> Int
findWeakness combo = (head (sort combo)) + (last (sort combo))

allContiguousSubsets :: Int -> [a] -> [[a]]
allContiguousSubsets _ [] = [[]]
allContiguousSubsets start list
  | start == length list - 1 = []
  | otherwise = [ drop start . take x $ list | x <- [1..(length list)] ] ++ allContiguousSubsets (start + 1) list

part2 = (findWeakness . findCombo incorrectNumber . map (\x -> read x::Int) . lines) <$> readFile "day9input.txt"