module IsSubsequenceOf where

import Data.Char

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf (x:xs) [] = False
isSubsequenceOf s@(x:xs) t@(y:ys) = undefined

capitalizeWords :: String -> [(String, String)]
capitalizeWords = go . words
  where
    go [] = []
    go ([]:_) = []
    go (w@(x:xs):ws) = (w, toUpper x : xs) : go ws

