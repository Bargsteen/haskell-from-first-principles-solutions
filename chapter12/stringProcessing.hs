module StringProcessing where

import Data.List

notThe :: String -> Maybe String
notThe s = if s' == "" then Nothing else Just s'
  where
    s' = concat $ intersperse " " $ filter (/= "the") $ words s

replaceThe :: String -> String
replaceThe = ( concat . intersperse " ". map replace . words)
  where
    replace = (\ x -> if x == "the" then "a" else x)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go (words s)
  where
    go [] = 0
    go (x:x'@(y:_):xs) = if x == "the" && isVowel y then 1 + go (x':xs) else go (x':xs)
    go _ = 0

countVowels :: String -> Integer
countVowels = (toInteger . length . filter isVowel)

isVowel :: Char -> Bool
isVowel = (flip elem) "aeiouy"

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = if consonantCount <= vowelCount
              then Just $ Word' s
              else Nothing
  where
    vowelCount = foldr (\ a b -> if isVowel a then 1 else b) 0 s
    consonantCount = length s - vowelCount
