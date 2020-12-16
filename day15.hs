import qualified Data.Map as Map

main = do
    let startingNumbers = [16,1,0,18,12,14,19]
    
    let startingIndex = (last startingNumbers, length startingNumbers, Map.fromList $ zip (init startingNumbers) [1..])

    print $ getNumberAtTurn 2020 startingIndex
    print $ getNumberAtTurn 30000000 startingIndex

getNumberAtTurn :: Int -> (Int, Int, Map.Map Int Int) -> Int
getNumberAtTurn turn (lastNumber, lastTurn, memory)
    | turn == lastTurn = lastNumber
    | otherwise = getNumberAtTurn turn (sayNextNumber (lastNumber, lastTurn, memory))

sayNextNumber :: (Int, Int, Map.Map Int Int) -> (Int, Int, Map.Map Int Int)
sayNextNumber (lastNumber, lastTurn, memory) = case Map.lookup lastNumber memory of
    Nothing -> (0, currentTurn, Map.insert lastNumber lastTurn memory)
    (Just lastSpokenOnTurn) -> (lastTurn - lastSpokenOnTurn, currentTurn, Map.insert lastNumber lastTurn memory)
    where currentTurn = lastTurn + 1
