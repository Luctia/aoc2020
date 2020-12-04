import Data.List
import Data.Char

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
  | xs == [] = [x ++ " " ++ elem]
  | otherwise = [x] ++ (addToLast elem xs)

countValidPassports :: [String] -> Int
countValidPassports [] = 0
countValidPassports (passport: passports)
  | isValidPassport passport = 1 + countValidPassports passports
  | otherwise = countValidPassports passports

isValidPassport :: String -> Bool
isValidPassport passport = isInfixOf "byr" passport && isInfixOf "iyr" passport && isInfixOf "eyr" passport && isInfixOf "hgt" passport && isInfixOf "hcl" passport && isInfixOf "ecl" passport && isInfixOf "pid" passport

-- Part 2
validNum :: String -> Int -> Int -> Bool
validNum input min max = all isDigit input && toInt >= min && toInt <= max
                         where toInt = read input :: Int

hgtValid :: String -> Bool
hgtValid hgt
  | isSuffixOf "in" hgt && length hgt == 4 = validIn hgt
  | isSuffixOf "cm" hgt && length hgt == 5 = validCm hgt
  | otherwise = False

validCm :: String -> Bool
validCm hgt = (length hgt) == 5 && toInt >= 150 && toInt <= 193
              where toInt = read (take 3 hgt) :: Int

validIn :: String -> Bool
validIn hgt = (length hgt) == 4 && toInt >= 59 && toInt <= 76
              where toInt = read (take 2 hgt) :: Int

eclValid :: String -> Bool
eclValid ecl = elem ecl ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

fieldValid :: String -> String -> Bool
fieldValid "byr" value = validNum value 1920 2002
fieldValid "iyr" value = validNum value 2010 2020
fieldValid "eyr" value = validNum value 2020 2030
fieldValid "hgt" value = hgtValid value
fieldValid "hcl" ('#':hcl) = all isHexDigit hcl && length hcl == 6
fieldValid "ecl" value = eclValid value
fieldValid "pid" value = length (map isDigit value) == 9 && all isDigit value
fieldValid "cid" _ = True
fieldValid _ _ = False

isValidPassport2 :: [String] -> Int -> Bool
isValidPassport2 [] seen = seen == 7
isValidPassport2 (element:elements) seen = fieldValid (take 3 element) (drop 4 element) && isValidPassport2 elements newSeen
                                           where newSeen
                                                    | take 3 element /= "cid" = seen + 1
                                                    | otherwise = seen

countValidPassports2 :: [String] -> Int
countValidPassports2 [] = 0
countValidPassports2 (passport:passports)
  | isValidPassport2 (words passport) 0 = 1 + countValidPassports2 passports
  | otherwise = countValidPassports2 passports

main = (countValidPassports2 . getIndividualPassports [] . lines) <$> readFile "day4input.txt"