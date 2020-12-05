import Data.List

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

main = (maximum . getAllSeatIDs . lines) <$> readFile "day5data.txt"

getAllSeatIDs :: [String] -> [Int]
getAllSeatIDs [] = []
getAllSeatIDs (boardingPass:boardingPasses) = calculateSeatID boardingPass : getAllSeatIDs boardingPasses

getMissingSeatID :: [Int] -> Int
getMissingSeatID (id1:id2:ids)
  | id1 + 1 == id2 = getMissingSeatID (id2:ids)
  | otherwise = id1 + 1

main2 = (getMissingSeatID . sort . getAllSeatIDs . lines) <$> readFile "day5data.txt"

maintest = (sort . getAllSeatIDs . lines) <$> readFile "day5data.txt"