import Data.List.Split

setMemory :: [(Int, Int)] -> Int -> Int -> [(Int, Int)]
setMemory [] location newValue = [(location, newValue)]
setMemory ((address, value):memory) location newValue
  | address == location = (address, newValue):memory
  | otherwise = (address, value):(setMemory memory location newValue)

getFromMemory :: [(Int, Int)] -> Int -> Int
getFromMemory [] _ = error "Value not found"
getFromMemory ((address, value):memory) index
  | address == index = value
  | otherwise = getFromMemory memory index

getMaskedValue :: String -> [Int] -> [Int]
getMaskedValue [] [] = []
getMaskedValue (m:mask) (v:value)
  | m == 'X' = v:(getMaskedValue mask value)
  | otherwise = (read [m]::Int):(getMaskedValue mask value)
getMaskedValue _ _ = error "Bitmask does not have the same length as input value"

-- decToBin outputs a list that is 36 elements long.
decToBin :: Int -> [Int]
decToBin 0 = replicate 36 0
decToBin n = (replicate (36 - (length outcome)) 0) ++ (reverse $ outcome)
           where outcome = decToBin' n

decToBin' :: Int -> [Int]
decToBin' 0 = []
decToBin' n = let (q, r) = n `divMod` 2 in r : decToBin' q

binToDec :: [Int] -> Int
binToDec bits = sum [ (snd zipped) * (2 ^ (fst zipped)) | zipped <- zip [0..((length bits) - 1)] (reverse bits) ]

walkThroughInput :: [(Int, Int)] -> String -> [String] -> [(Int, Int)]
walkThroughInput memory _ [] = memory
walkThroughInput memory mask (inst:insts)
  | take 3 inst == "mas" = walkThroughInput memory (drop 7 inst) insts
  | otherwise = walkThroughInput newMemory mask insts
  where newMemory = setMemory memory location (binToDec (getMaskedValue mask (decToBin newValue)))
        splitted = splitOn " = " inst
        newValue = read (splitted !! 1)::Int
        location = read (init (drop 4 (splitted !! 0)))::Int

getMemorySum :: [(Int, Int)] -> Int
getMemorySum memory = sum $ map snd memory

part1 = (getMemorySum . walkThroughInput [] "" . lines) <$> readFile "day14input.txt"

allPosibilities :: [Int] -> String -> [[Int]] -> [[Int]]
allPosibilities [] [] completed = completed
allPosibilities (i:input) (m:mask) completed
  | m == '0' = allPosibilities input mask (map (++[i]) completed)
  | m == '1' = allPosibilities input mask (map (++[(read [m]::Int)]) completed)
  | m == 'X' = allPosibilities input mask ((map (++[1]) completed) ++ (map (++[0]) completed))
allPosibilities _ _ _ = error "Bitmask does not have the same length as input value"

walkThroughInput2 :: [(Int, Int)] -> String -> [String] -> [(Int, Int)]
walkThroughInput2 memory _ [] = memory
walkThroughInput2 memory mask (inst:insts)
  | take 3 inst == "mas" = walkThroughInput2 memory (drop 7 inst) insts
  | otherwise = walkThroughInput2 (addToMemory ++ memory) mask insts
  where splitted = splitOn " = " inst
        newValue = read (splitted !! 1)::Int
        location = read (init (drop 4 (splitted !! 0)))::Int
        allpos = map (binToDec) (allPosibilities (decToBin location) mask [[]])
        addToMemory = [ (pos, newValue) | pos <- allpos ]

getMemorySum2 :: Int -> [(Int, Int)] -> Int
getMemorySum2 _ [] = 0
getMemorySum2 previous (m:memory)
  | (fst m) == previous = getMemorySum2 previous memory
  | otherwise = (snd m) + getMemorySum2 (fst m) memory

part2 = (getMemorySum2 (-1) . walkThroughInput2 [] "" . lines) <$> readFile "day14input.txt"