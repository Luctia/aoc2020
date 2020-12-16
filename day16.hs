import Data.List.Split

data Rule = Rule String [(Int, Int)] deriving (Show, Eq)

constructRule :: String -> Rule
constructRule input = Rule (head nameRanges) [ (read min::Int, read max::Int) | range <- ranges, let minMax = splitOn "-" range, let min = head minMax, let max = last minMax ]
                    where nameRanges = splitOn ":" input
                          ranges = splitOn " or " $ last nameRanges

computeTSER :: [Rule] -> [Int] -> Int
computeTSER _ [] = 0
computeTSER rules (i:is)
  | matchingRules == [] = i + (computeTSER rules is)
  | otherwise = computeTSER rules is
  where matchingRules = getMatchingRules rules i

getMatchingRules :: [Rule] -> Int -> [Rule]
getMatchingRules [] _ = []
getMatchingRules (rule:rules) i
  | matchesRule rule i = rule:(getMatchingRules rules i)
  | otherwise = getMatchingRules rules i

matchesRule :: Rule -> Int -> Bool
matchesRule (Rule name ranges) i = any (==True) [ i >= (fst range) && i <= (snd range) | range <- ranges ]

getAllInts :: [String] -> [Int]
getAllInts input = map (\x -> read x::Int) (foldr (++) [] (map (splitOn "," ) (tail input)))

-- part1 = ((\x -> ((map constructRule (head x)), (getAllInts (last x)))) . splitOn [""] . lines) <$> readFile "day16input.txt"
part1 = ((\x -> computeTSER (map constructRule (head x)) (getAllInts (last x))) . splitOn [""] . lines) <$> readFile "day16input.txt"