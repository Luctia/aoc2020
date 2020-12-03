gaanMetDieBanaan :: Int -> Int -> Int -> Int -> [String] -> Int
gaanMetDieBanaan right down col line inputlines
  | line >= length inputlines = 0
  | inputlines!!line!!col == '#' = 1 + (gaanMetDieBanaan right down ((col + right) `mod` 31) (line + down) inputlines)
  | otherwise = gaanMetDieBanaan right down ((col + right) `mod` 31) (line + down) inputlines

part1 = (gaanMetDieBanaan 3 1 0 0 . lines) <$> readFile "day3input.txt"