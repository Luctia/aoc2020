data Program = Program [Operation]
             deriving (Show, Eq)

data Operation = Acc Int
               | Jmp Int
               | Nop Int
               | Finished
               deriving (Show, Eq)

convertToOperation :: String -> Operation
convertToOperation input
  | take 3 input == "acc" = Acc int
  | take 3 input == "jmp" = Jmp int
  | otherwise = Nop int
  where neg = input !! 4 == '-'
        int = if neg then -1 * raw else raw
        raw = (read $ drop 5 input)

executeProgramButStopAfter1Loop' :: Program -> Operation -> [Int] -> Int -> Int -> Int
executeProgramButStopAfter1Loop' _ (Finished) _ _ accumulator = accumulator
executeProgramButStopAfter1Loop' (Program ops) (Nop _) seen pointer accumulator
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

executeProgramButStopAfter1Loop :: Program -> Int
executeProgramButStopAfter1Loop (Program program) = executeProgramButStopAfter1Loop' (Program program) (program !! 0) [] 0 0

operationsToProgram :: [Operation] -> Program
operationsToProgram ops = Program ops

part1 = (executeProgramButStopAfter1Loop . operationsToProgram . map (convertToOperation) . lines) <$> readFile "day8input.txt"

executeProgramAndDetectLoop' :: Program -> Operation -> [Int] -> Int -> Int -> Bool
executeProgramAndDetectLoop' _ (Finished) _ _ accumulator = False
executeProgramAndDetectLoop' (Program ops) (Nop _) seen pointer accumulator
  | elem pointer seen = True
  | otherwise = executeProgramAndDetectLoop' (Program ops) (getOperation ops (pointer + 1)) ((pointer):seen) (pointer + 1) accumulator
executeProgramAndDetectLoop' (Program ops) (Acc x) seen pointer accumulator
  | elem pointer seen = True
  | otherwise = executeProgramAndDetectLoop' (Program ops) (getOperation ops (pointer + 1)) ((pointer):seen) (pointer + 1) (accumulator + x)
executeProgramAndDetectLoop' (Program ops) (Jmp x) seen pointer accumulator
  | elem pointer seen = True
  | otherwise = executeProgramAndDetectLoop' (Program ops) (getOperation ops (pointer + x)) ((pointer):seen) (pointer + x) accumulator

executeProgramAndDetectLoop :: Program -> Bool
executeProgramAndDetectLoop (Program program) = executeProgramAndDetectLoop' (Program program) (program !! 0) [] 0 0

executeProgram' :: Program -> Operation -> Int -> Int -> Int
executeProgram' _ (Finished) _ accumulator = accumulator
executeProgram' (Program ops) (Nop _) pointer accumulator = executeProgram' (Program ops) (getOperation ops (pointer + 1)) (pointer + 1) accumulator
executeProgram' (Program ops) (Acc x) pointer accumulator = executeProgram' (Program ops) (getOperation ops (pointer + 1)) (pointer + 1) (accumulator + x)
executeProgram' (Program ops) (Jmp x) pointer accumulator = executeProgram' (Program ops) (getOperation ops (pointer + x)) (pointer + x) accumulator

executeProgram :: Program -> Int
executeProgram (Program program) = executeProgram' (Program program) (program !! 0) 0 0

swapNopsJmps :: Program -> [Program]
swapNopsJmps (Program program) = unique [ Program prog | prog <- (map (swapIfNopOrJmp program) [0..(length program)])]

swapIfNopOrJmp :: [Operation] -> Int -> [Operation]
swapIfNopOrJmp [] _ = []
swapIfNopOrJmp ((Nop x):ops) 0 = (Jmp x):ops
swapIfNopOrJmp ((Jmp x):ops) 0 = (Nop x):ops
swapIfNopOrJmp (op:ops) i
  | i > 0 = op:(swapIfNopOrJmp ops (i - 1))
  | otherwise = op:ops

unique :: (Eq a) => [a] -> [a]
unique list = unique' list []

unique' :: (Eq a) => [a] -> [a] -> [a]
unique' [] _ = []
unique' (element:list) seen
  | elem element seen = unique' list seen
  | otherwise = element:(unique' list (element:seen))

findCorrectSwap :: [Program] -> Int
findCorrectSwap (prog:progs)
  | executeProgramAndDetectLoop prog  = findCorrectSwap progs
  | otherwise = executeProgram prog

part2 = (findCorrectSwap . swapNopsJmps . operationsToProgram . map (convertToOperation) . lines) <$> readFile "day8input.txt"