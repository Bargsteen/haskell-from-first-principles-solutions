module WordNumber where

import           Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n =
  case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    _ -> "ERR"

digits :: Int -> [Int]
digits n
  | n <= 9 = [n]
  | otherwise = digits start ++ digits lastD
  where
    start = div n 10
    lastD = mod n 10

wordNumber :: Int -> String
wordNumber n = intercalate "-" (map digitToWord $ digits n)
