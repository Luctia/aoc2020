import Data.List
import Data.Maybe

data Direction = North | East | South | West deriving (Show, Eq)

data State = State (Int, Int) Direction deriving (Show)

data NavInst = NavInstMove Direction Int
             | NavInstTurn Char Int
             | NavInstForward Int

charToDirection :: Char -> Direction
charToDirection c
  | c == 'N' = North
  | c == 'E' = East
  | c == 'S' = South
  | otherwise = West

stringToInstructions :: String -> NavInst
stringToInstructions (l:n)
  | l == 'L' || l == 'R' = NavInstTurn l value
  | l == 'F' = NavInstForward value
  | otherwise = NavInstMove direction value
  where value = read n::Int
        direction = charToDirection l

getNewState :: State -> NavInst -> State
getNewState (State (x, y) directionFacing) (NavInstForward steps)
  | directionFacing == North = State (x, y + steps) directionFacing
  | directionFacing == East = State (x + steps, y) directionFacing
  | directionFacing == South = State (x, y - steps) directionFacing
  | otherwise = State (x - steps, y) directionFacing
getNewState (State (x, y) directionFacing) (NavInstMove direction steps)
  | direction == North = State (x, y + steps) directionFacing
  | direction == East = State (x + steps, y) directionFacing
  | direction == South = State (x, y - steps) directionFacing
  | otherwise = State (x - steps, y) directionFacing
getNewState (State (x, y) directionFacing) (NavInstTurn direction degrees) = State (x, y) (getNewDirection direction degrees directionFacing)

getNewDirection :: Char -> Int -> Direction -> Direction
getNewDirection direction degrees directionFacing
  | direction == 'R' = directions !! mod (currentIndex + noSteps) 4
  | otherwise = directions !! mod (currentIndex - noSteps) 4
  where noSteps = div degrees 90
        directions = [ North, East, South, West ]
        currentIndex = fromJust $ elemIndex directionFacing directions

executeInstructions :: State -> [NavInst] -> State
executeInstructions state [] = state
executeInstructions state (inst:insts) = executeInstructions (getNewState state inst) insts

calculateManhattanDistance :: State -> Int
calculateManhattanDistance (State (x, y) _) = abs x + abs y

part1 = (calculateManhattanDistance . executeInstructions (State (0, 0) East) . map stringToInstructions . lines) <$> readFile "day12input.txt"

data WPInst = WPMove Direction Int
            | WPTurn Char Int
            | WPForward Int
            deriving (Show)

data WPState = WPState (Int, Int) (Int, Int) deriving (Show)

stringToWPInst :: String -> WPInst
stringToWPInst (c:v)
  | c == 'F' = WPForward value
  | c == 'L' || c == 'R' = WPTurn c value
  | otherwise = WPMove (charToDirection c) value
  where value = read v::Int

rotate :: (Int, Int) -> Int -> (Int, Int)
rotate pair 0 = pair
rotate (x, y) i = rotate (y, -x) (i - 1)

executeWpInst :: WPState -> WPInst -> WPState
executeWpInst (WPState (bx, by) (wx, wy)) (WPMove direction steps)
  | direction == North = WPState (bx, by) (wx, wy + steps)
  | direction == East = WPState (bx, by) (wx + steps, wy)
  | direction == South = WPState (bx, by) (wx, wy - steps)
  | otherwise = WPState (bx, by) (wx - steps, wy)
executeWpInst (WPState (bx, by) (wx, wy)) (WPTurn direction degrees)
  | direction == 'R' = WPState (bx, by) (rotate (wx, wy) noSteps)
  | otherwise = WPState (bx, by) (rotate (wx, wy) noStepsCC)
  where noSteps = mod (div degrees 90) 4
        noStepsCC = if mod noSteps 2 == 0 then noSteps else noSteps + 2
executeWpInst (WPState (bx, by) (wx, wy)) (WPForward steps) = WPState (bx + wx * steps, by + wy * steps) (wx, wy)

executeWpInsts :: WPState -> [WPInst] -> WPState
executeWpInsts state [] = state
executeWpInsts state (inst:insts) = executeWpInsts (executeWpInst state inst) insts

calculateManhattanDistance2 :: WPState -> Int
calculateManhattanDistance2 (WPState (x, y) _) = abs x + abs y

part2 = (calculateManhattanDistance2 . executeWpInsts (WPState (0, 0) (10, 1)) . map stringToWPInst . lines) <$> readFile "day12input.txt"