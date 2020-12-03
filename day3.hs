gaanMetDieBanaan :: Int -> Int -> Int -> Int -> [String] -> Int
gaanMetDieBanaan right down col line inputlines
  | line >= length inputlines = 0
  | inputlines!!line!!col == '#' = 1 + (gaanMetDieBanaan right down ((col + right) `mod` 31) (line + down) inputlines)
  | otherwise = gaanMetDieBanaan right down ((col + right) `mod` 31) (line + down) inputlines

part1 = (gaanMetDieBanaan 3 1 0 0 . lines) <$> readFile "day3input.txt"

steps :: [(Int, Int)]
steps = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

getAll :: [String] -> [Int]
getAll input = [ res | (right, down) <- steps, let res = gaanMetDieBanaan right down 0 0 input ]

part2 = (foldl (*) 1 . getAll . lines) <$> readFile "day3input.txt"