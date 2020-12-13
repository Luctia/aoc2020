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