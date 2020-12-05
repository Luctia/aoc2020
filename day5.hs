testBoardingPasses = ["BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL"]

calculateSeatID :: String -> Int
calculateSeatID boardingPass = (calculateRow (take 7 boardingPass) 0 127) * 8 + calculateColumn (drop 7 boardingPass) 0 7

calculateRow :: String -> Int -> Int -> Int
calculateRow [] min _ = min
calculateRow (char:chars) min max
  | char == 'F' = calculateRow chars min $range!!(div maxIndex 2)
  | otherwise = calculateRow chars (range!!(1 + (div maxIndex 2))) max
  where range = [min..max]
        maxIndex = length range - 1

calculateColumn :: String -> Int -> Int -> Int
calculateColumn [] min _ = min
calculateColumn (char:chars) min max
  | char == 'L' = calculateColumn chars min $range!!(div maxIndex 2)
  | otherwise = calculateColumn chars (range!!(1 + (div maxIndex 2))) max
  where range = [min..max]
        maxIndex = length range - 1

getHighestSeatID :: Int -> [String] -> Int
getHighestSeatID max [] = max
getHighestSeatID max (boardingPass:boardingPasses)
  | seatID > max = getHighestSeatID seatID boardingPasses
  | otherwise = getHighestSeatID max boardingPasses
  where seatID = calculateSeatID boardingPass

main = (getHighestSeatID (-1) . lines) <$> readFile "day5data.txt"