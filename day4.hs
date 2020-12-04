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
byrValid :: String -> Bool
byrValid byr = all isDigit byr && toInt >= 1920 && toInt <= 2002
               where toInt = read byr :: Int

iyrValid :: String -> Bool
iyrValid iyr = all isDigit iyr && toInt >= 2010 && toInt <= 2020
               where toInt = read iyr :: Int

eyrValid :: String -> Bool
eyrValid eyr = all isDigit eyr && toInt >= 2020 && toInt <= 2030
               where toInt = read eyr :: Int

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

hclValid :: String -> Bool
hclValid ('#':hcl) = all isHexDigit hcl && length hcl == 6
hclValid _ = False

eclValid :: String -> Bool
eclValid ecl = elem ecl ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

pidValid :: String -> Bool
pidValid pid = length (map isDigit pid) == 9 && all isDigit pid

countValidPassports2 :: [String] -> Int
countValidPassports2 [] = 0
countValidPassports2 (passport:passports)
  | isValidPassport2 (words passport) [] = 1 + countValidPassports2 passports
  | otherwise = countValidPassports2 passports

isValidPassport2 :: [String] -> [String] -> Bool
isValidPassport2 [] seen = length seen == 7
isValidPassport2 (element:elements) seen
  | take 3 element == "byr" && byrValid (drop 4 element) && not (elem "byr" seen) = isValidPassport2 elements (seen ++ ["byr"])
  | take 3 element == "iyr" && iyrValid (drop 4 element) && not (elem "iyr" seen) = isValidPassport2 elements (seen ++ ["iyr"])
  | take 3 element == "eyr" && eyrValid (drop 4 element) && not (elem "eyr" seen) = isValidPassport2 elements (seen ++ ["eyr"])
  | take 3 element == "hgt" && hgtValid (drop 4 element) && not (elem "hgt" seen) = isValidPassport2 elements (seen ++ ["hgt"])
  | take 3 element == "hcl" && hclValid (drop 4 element) && not (elem "hcl" seen) = isValidPassport2 elements (seen ++ ["hcl"])
  | take 3 element == "ecl" && eclValid (drop 4 element) && not (elem "ecl" seen) = isValidPassport2 elements (seen ++ ["ecl"])
  | take 3 element == "pid" && pidValid (drop 4 element) && not (elem "pid" seen) = isValidPassport2 elements (seen ++ ["pid"])
  | take 3 element == "cid" = isValidPassport2 elements seen
  | otherwise = False

main = (countValidPassports2 . getIndividualPassports [] . lines) <$> readFile "day4input.txt"