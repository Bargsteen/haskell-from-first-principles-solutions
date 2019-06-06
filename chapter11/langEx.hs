module LanguageExercises where

import Data.Char
import Data.List

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph (x:xs) = toUpper x : go xs
  where
    go "" = ""
    go ('.':' ':y:ys) = ". " ++ [(toUpper y)] ++ go ys
    go (z:zs) = z : go zs

capitalizeParagraph' :: String -> String
capitalizeParagraph' "" = ""
capitalizeParagraph' (x:xs) = concat $ intersperse " " $ go $ words (toUpper x : xs)
  where
    go [] = []
    go (y1:y2:ys) = if elem '.' y1
                    then y1 : (capitalizeWord y2) : go ys
                    else y1 : y2 : ys
    go (z:zs) = z : zs
