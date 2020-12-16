import Data.List.Split (splitOn)
import Data.List (transpose, intersect, sortBy, delete, isPrefixOf)
import Data.Function (on)

data Rule = Rule String [(Int, Int)] deriving (Show, Eq, Ord)

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

part1 = ((\x -> computeTSER (map constructRule (head x)) (getAllInts (last x))) . splitOn [""] . lines) <$> readFile "day16input.txt"

getValidTickets :: [Rule] -> [[Int]] -> [[Int]]
getValidTickets _ [] = []
getValidTickets rules (ticket:tickets)
  | any (==[]) (map (getMatchingRules rules) ticket) = getValidTickets rules tickets
  | otherwise = ticket:(getValidTickets rules tickets)

getPossibleFields :: [[String]] -> [(Int, [String])]
getPossibleFields input = zip [0..] [ map (\(Rule name rs) -> name) res | ruleValues <- fieldsOfARule, let matchingRules = map (getMatchingRules rules) ruleValues, let res = foldr intersect rules matchingRules ]
                          where rules = map (constructRule) (head input)
                                nearbyTickets = map (splitOn ",") (tail (input !! 2))
                                nearbyTicketsInts = [ map (\x -> read x::Int) string | string <- nearbyTickets ]
                                nearbyTicketsValid = getValidTickets rules nearbyTicketsInts
                                fieldsOfARule = transpose nearbyTicketsValid

determineFieldOrder :: [(Int, [String])] -> [(Int, String)]
determineFieldOrder combos = determineFieldOrder' (sortOptions combos) []

determineFieldOrder' :: [(Int, [String])] -> [(Int, String)] -> [(Int, String)]
determineFieldOrder' [] res = res
determineFieldOrder' (combo:combos) res
  | length (snd combo) == 1 = determineFieldOrder' [ (i, delete (head $ snd combo) c) | (i, c) <- combos ] ((fst combo, head $ snd combo):res)
  | otherwise = determineFieldOrder' (sortOptions combos) res

sortOptions :: [(a, [b])] -> [(a, [b])]
sortOptions = sortBy (compare `on` length . snd)

getAnswer :: [(Int, String)] -> [Int] -> Int
getAnswer fields values = foldr (*) 1 [ values !! i | i <- [0..length fields - 1], isPrefixOf "departure" (snd (sorted !! i)) ]
                        where sorted = sortBy (compare `on` fst) fields

part2 = ((\x -> getAnswer (determineFieldOrder $ getPossibleFields x) (map (\y -> read y::Int) (splitOn "," (x!!1!!1)))) . splitOn [""] . lines) <$> readFile "day16input.txt"