import Data.List

countDifferences :: [(Int, Int)] -> [Int] -> [(Int, Int)]
countDifferences diffs [x,y] = incrementDiffCount (y - x) includeFirst
                             where includeFinal = incrementDiffCount 3 diffs
                                   includeFirst = incrementDiffCount 1 includeFinal
countDifferences diffs (x:y:xs) = countDifferences (incrementDiffCount (y - x) diffs) (y:xs)

incrementDiffCount :: Int -> [(Int, Int)] -> [(Int, Int)]
incrementDiffCount diff [] = [(diff, 1)]
incrementDiffCount diff ((key, value):sets)
  | diff == key = (key, value + 1):sets
  | otherwise = (key, value):(incrementDiffCount diff sets)

lookfor :: (Eq a) => a -> [(a, b)] -> b
lookfor needle ((key, value):maps)
  | needle == key = value
  | otherwise = lookfor needle maps

part1 = ((\x -> (lookfor 1 x) * (lookfor 3 x)) . countDifferences [] . sort . map (\x -> read x::Int) . lines) <$> readFile "day10data.txt"

countAllPosLinks :: [Int] -> Int
countAllPosLinks sockets = last $ countAllPosLinks' 1 paddedSockets [1]
  where paddedSockets = [0] ++ sockets ++ [last sockets + 3]

countAllPosLinks' :: Int -> [Int] -> [Int] -> [Int]
countAllPosLinks' index sockets preds
  | index >= length sockets = preds
  | otherwise = countAllPosLinks' (index + 1) sockets (preds ++ [linksToHere])
  where linksToHere = sum [ preds!!x | x <- [(index - 4)..(index - 1)], x >= 0, sockets!!x >= sockets!!index - 3 ]

part2 = (countAllPosLinks . sort . map (\x -> read x::Int) . lines) <$> readFile "day10data.txt"