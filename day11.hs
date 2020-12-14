import Data.List

-- Pad the grid with empty spots all around it
padGrid :: [String] -> [String]
padGrid grid = [(replicate width '.')] ++ (transpose $ [(replicate height '.')] ++ (transpose grid) ++ [(replicate height '.')]) ++ [(replicate width '.')]
              where width = 2 + (length $ head grid)
                    height = length grid

getNeighbours :: (Int, Int) -> [String] -> [Char]
getNeighbours (x, y) grid = [ grid!!neighY!!neighX | neighX <- [x-1..x+1], neighY <- [y-1..y+1], not (x == neighX && y == neighY) ]

iterateSquare :: (Int, Int) -> [String] -> Char
iterateSquare (x, y) grid
  | me == 'L' && occupied == 0 = '#'
  | me == '#' && occupied > 3 = 'L'
  | otherwise = me
  where me = grid!!y!!x
        occupied = length $ filter (=='#') $ getNeighbours (x, y) grid

iterateGrid :: [String] -> [String]
iterateGrid grid = padGrid [ [ iterateSquare (x, y) grid | x <- [1..(length (grid!!0)) - 2] ] | y <- [1..(length grid) - 2] ]

iterateTillDone :: [String] -> [String]
iterateTillDone prev
  | new == prev = new
  | otherwise = iterateTillDone new
  where new = iterateGrid prev

countSeats :: [String] -> Int
countSeats [] = 0
countSeats (line:lines) = (length $ filter (=='#') line) + (countSeats lines)

-- displayGrid :: [String]
displayGrid lines = mapM_ print lines

part1 = (countSeats . iterateTillDone . padGrid . lines) <$> readFile "day11input.txt"