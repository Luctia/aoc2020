import Data.List

getIndividualPassports :: [String] -> [String] -> [String]
getIndividualPassports res input = getIndividualPassports' res True input

getIndividualPassports' :: [String] -> Bool -> [String] -> [String]
getIndividualPassports' res _ [] = res
getIndividualPassports' res new (input:inputs)
  | input == "" = getIndividualPassports' res True inputs
  | new = getIndividualPassports' (res ++ [input]) False inputs
  | otherwise = getIndividualPassports' newRes False inputs
  where newRes = addToLast input res


addToLast :: String -> [String] -> [String]
addToLast elem [] = [elem]
addToLast elem (x:xs)
  | xs == [] = [x ++ elem]
  | otherwise = [x] ++ (addToLast elem xs)

countValidPassports :: [String] -> Int
countValidPassports [] = 0
countValidPassports (passport: passports)
  | isValidPassport passport = 1 + countValidPassports passports
  | otherwise = countValidPassports passports

isValidPassport :: String -> Bool
isValidPassport passport = isInfixOf "byr" passport && isInfixOf "iyr" passport && isInfixOf "eyr" passport && isInfixOf "hgt" passport && isInfixOf "hcl" passport && isInfixOf "ecl" passport && isInfixOf "pid" passport

main = (countValidPassports . getIndividualPassports [] . lines) <$> readFile "day4input.txt"