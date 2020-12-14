import Data.List.Split

data Rule = Rule String Content
          deriving (Show, Eq)

data Content = Content [(Int, String)]
             deriving (Show, Eq)

getRule :: String -> Rule
getRule input = Rule (split !! 0) $ getContent $ (split !! 1)
              where split = splitOn " bags contain " $ init input

getContent :: String -> Content
getContent "no other bags" = Content []
getContent input = Content [ (read ((words contents)!!0)::Int, drop 2 contents) | contents <- map (!! 0) (map (splitOn " bag") (splitOn ", " input)) ]

findDirectContainers :: String -> [Rule] -> [String]
findDirectContainers _ [] = []
findDirectContainers needle ((Rule container contents):rules)
  | contains contents needle = [container] ++ (findDirectContainers needle rules)
  | otherwise = findDirectContainers needle rules

contains :: Content -> String -> Bool
contains (Content []) _ = False
contains (Content ((num, bag):cs)) needle
  | bag == needle = True
  | otherwise = contains (Content cs) needle

findAllContainers :: String -> [Rule] -> [String]
findAllContainers needle rules
  | directContainers == [] = []
  | otherwise = unique (foldl (++) directContainers (map (\x -> findAllContainers x rules) directContainers))
  where directContainers = findDirectContainers needle rules

unique :: (Eq a) => [a] -> [a]
unique list = unique' list []

unique' :: (Eq a) => [a] -> [a] -> [a]
unique' [] _ = []
unique' (element:list) seen
  | elem element seen = unique' list seen
  | otherwise = element:(unique' list (element:seen))

part1 = (length . findAllContainers "shiny gold" . map getRule . lines) <$> readFile "day7input.txt"

findRule :: String -> [Rule] -> Rule
findRule _ [] = Rule "" (Content [])
findRule container ((Rule container' contents):rules)
  | container' == container = Rule container' contents
  | otherwise = findRule container rules

countTotalContents :: String -> [Rule] -> Int
countTotalContents needle rules = (countTotalContents' needle rules) - 1

countTotalContents' :: String -> [Rule] -> Int
countTotalContents' needle rules
  | contents == [] = 1
  | otherwise = 1 + sum [ amount * (countTotalContents' container rules) | (amount, container) <- contents ]
  where (Rule from (Content contents)) = findRule needle rules

part2 = (countTotalContents "shiny gold" . map getRule . lines) <$> readFile "day7input.txt"