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