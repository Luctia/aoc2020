data Program = Program [Operation] deriving Show

data Operation = Acc Int
               | Jmp Int
               | Nop
               | Finished
               deriving (Show)

convertToOperation :: String -> Operation
convertToOperation input
  | take 3 input == "acc" = Acc int
  | take 3 input == "jmp" = Jmp int
  | otherwise = Nop
  where neg = input !! 4 == '-'
        int = if neg then -1 * raw else raw
        raw = (read $ drop 5 input)

-- executeProgram' :: Program -> Operation -> Int -> Int -> Int
-- executeProgram' _ (Finished) _ accumulator = accumulator
-- executeProgram' (Program ops) (Nop) pointer accumulator = executeProgram' (Program ops) (getOperation ops (pointer + 1)) (pointer + 1) accumulator
-- executeProgram' (Program ops) (Acc x) pointer accumulator = executeProgram' (Program ops) (getOperation ops (pointer + 1)) (pointer + 1) (accumulator + x)
-- executeProgram' (Program ops) (Jmp x) pointer accumulator = executeProgram' (Program ops) (getOperation ops (pointer + x)) (pointer + x) accumulator

executeProgramButStopAfter1Loop' :: Program -> Operation -> [Int] -> Int -> Int -> Int
executeProgramButStopAfter1Loop' _ (Finished) _ _ accumulator = accumulator
executeProgramButStopAfter1Loop' (Program ops) (Nop) seen pointer accumulator
  | elem pointer seen = accumulator
  | otherwise = executeProgramButStopAfter1Loop' (Program ops) (getOperation ops (pointer + 1)) ((pointer):seen) (pointer + 1) accumulator
executeProgramButStopAfter1Loop' (Program ops) (Acc x) seen pointer accumulator
  | elem pointer seen = accumulator
  | otherwise = executeProgramButStopAfter1Loop' (Program ops) (getOperation ops (pointer + 1)) ((pointer):seen) (pointer + 1) (accumulator + x)
executeProgramButStopAfter1Loop' (Program ops) (Jmp x) seen pointer accumulator
  | elem pointer seen = accumulator
  | otherwise = executeProgramButStopAfter1Loop' (Program ops) (getOperation ops (pointer + x)) ((pointer):seen) (pointer + x) accumulator

getOperation :: [Operation] -> Int -> Operation
getOperation [] _ = Finished
getOperation (op:ops) 0 = op
getOperation (op:ops) i = getOperation ops (i - 1)

executeProgram :: Program -> Int
executeProgram (Program program) = executeProgramButStopAfter1Loop' (Program program) (program !! 0) [] 0 0

operationsToProgram :: [Operation] -> Program
operationsToProgram ops = Program ops

main = (executeProgram . operationsToProgram . map (convertToOperation) . lines) <$> readFile "day8data.txt"