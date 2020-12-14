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

getVisibleSeats :: (Int, Int) -> [String] -> [Char]
getVisibleSeats (x, y) grid = [ getFirst dirList grid | dirList <- getAllDirectionLists (x, y) grid ]

getAllDirectionLists :: (Int, Int) -> [String] -> [[(Int, Int)]]
getAllDirectionLists (rx, ry) grid = [[ (rx, someY) | someY <- reverse [minY..ry-1] ],  -- Up
                                      [ (rx, someY) | someY <- [ry+1..maxY] ],          -- Down
                                      [ (someX, ry) | someX <- reverse [minX..rx-1] ],  -- Left
                                      [ (someX, ry) | someX <- [rx+1..maxX]],           -- Right
                                      [ (xs!!i, ys!!i) | let xs = [rx+1..maxX],         let ys = reverse [minY..ry-1],  i <- [0..(min (length xs) (length ys)) - 1] ],  -- Right up
                                      [ (xs!!i, ys!!i) | let xs = [rx+1..maxX],         let ys = [ry+1..maxY],          i <- [0..(min (length xs) (length ys)) - 1] ],  -- Right down
                                      [ (xs!!i, ys!!i) | let xs = reverse [minX..rx-1], let ys = [ry+1..maxY],          i <- [0..(min (length xs) (length ys)) - 1] ],  -- Left down
                                      [ (xs!!i, ys!!i) | let xs = reverse [minX..rx-1], let ys = reverse [minY..ry-1],  i <- [0..(min (length xs) (length ys)) - 1] ]]  -- Left up
                                   where minY = 1
                                         minX = 1
                                         maxY = length grid - 2
                                         maxX = length (grid!!0) - 2

getFirst :: [(Int, Int)] -> [String] -> Char
getFirst [] _ = '.'
getFirst (coord:coords) grid
  | grid!!(snd coord)!!(fst coord) /= '.' = grid!!(snd coord)!!(fst coord)
  | otherwise = getFirst coords grid

iterateSquare2 :: (Int, Int) -> [String] -> Char
iterateSquare2 (x, y) grid
  | me == 'L' && occupied == 0 = '#'
  | me == '#' && occupied > 4 = 'L'
  | otherwise = me
  where me = grid!!y!!x
        occupied = length $ filter (=='#') (getVisibleSeats (x, y) grid)

iterateGrid2 :: [String] -> [String]
iterateGrid2 grid = padGrid [ [ iterateSquare2 (x, y) grid | x <- [1..(length (grid!!0)) - 2] ] | y <- [1..(length grid) - 2] ]

iterateTillDone2 :: [String] -> [String]
iterateTillDone2 prev
  | new == prev = new
  | otherwise = iterateTillDone2 new
  where new = iterateGrid2 prev

part2 = (countSeats . iterateTillDone2 . padGrid . lines) <$> readFile "day11input.txt"