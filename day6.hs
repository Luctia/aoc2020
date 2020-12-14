import Data.List.Split
import Data.List

listToGroups :: String -> [String]
listToGroups list = map (deleteAll '\n') (map sort (splitOn "\n\n" list))

deleteAll :: (Eq a) => a -> [a] -> [a]
deleteAll _ [] = []
deleteAll toDelete (element:elements)
  | toDelete == element = deleteAll toDelete elements
  | otherwise = (element:(deleteAll toDelete elements))

countUnique :: String -> Int
countUnique [x] = 1
countUnique [x, y]
  | x == y = 1
  | otherwise = 2
countUnique (x:y:xs)
  | x == y = countUnique (y:xs)
  | otherwise = 1 + countUnique (y:xs)

getSumCounts :: String -> Int
getSumCounts input = sum [ countUnique x | x <- listToGroups input ]

main = getSumCounts <$> readFile "day6input.txt"

getSumInCommon :: String -> Int
getSumInCommon input = sum [ countInCommon x | x <- listToPeople input ]

listToPeople :: String -> [[String]]
listToPeople list = map (splitOn "\n") (splitOn "\n\n" list)

countInCommon :: [String] -> Int
countInCommon input = length $ commonElements (map sort input)

commonElements :: (Eq a) => [[a]] -> [a]
commonElements [x] = x
commonElements [x, y]
  | length x > length y = intersect x y
  | otherwise = intersect y x
commonElements (x:y:xs) = commonElements ((commonElements [x, y]):xs)

main2 = getSumInCommon <$> readFile "day6input.txt"