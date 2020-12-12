numberIsValid :: [Int] -> Int -> Bool
numberIsValid list index
  | index < 25 = True
  | elem (list !! index) (allAditions subset) = True
  | otherwise = False
  where subset = take 25 . drop (index - 25) $ list

allAditions :: [Int] -> [Int]
allAditions list = [ x + y | x <- list, y <- list]

getFirstMistake :: Int -> [Int] -> Int
getFirstMistake index list
  | numberIsValid list index = getFirstMistake (index + 1) list
  | otherwise = list !! index

part1 = (getFirstMistake 25 . map (\x -> read x::Int) . lines) <$> readFile "day9input.txt"