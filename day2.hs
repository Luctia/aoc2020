module Main where

import Day2input
import Data.List

main = do
    print (part1)
    print (part2)

part1 :: Int
part1 = length [ x | x <- getdata, isValid1 x ]

isValid1 :: (Int, Int, Char, String) -> Bool
isValid1 (start, end, char, input) = occ >= start && occ <= end where occ = countElem char input

part2 :: Int
part2 = length [ x | x <- getdata, isValid2 x ]

isValid2 :: (Int, Int, Char, String) -> Bool
isValid2 (first, second, char, input) = xor ((input!!(first - 1)) == char) ((input!!(second - 1)) == char)

countElem :: (Eq a) => a -> [a] -> Int
countElem needle haystack = length [ x | x <- haystack, x == needle ]

xor :: Bool -> Bool -> Bool
xor stat1 stat2 = (stat1 /= stat2) && (stat1 || stat2)