module Exercises1 where

import Data.Char

filterUpper :: String -> String
filterUpper s = filter isUpper s

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs

allCaps :: String -> String
allCaps [] = []
allCaps (x:xs) = toUpper x : allCaps xs

capHead :: String -> Char
capHead [] = ' '
capHead s = toUpper $ head s

capHeadComp :: String -> Char
capHeadComp s = (toUpper . head) s

capHeadPF :: String -> Char
capHeadPF = (toUpper . head)



