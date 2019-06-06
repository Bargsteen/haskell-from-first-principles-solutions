module Intermission2 where

myWords :: String -> [String]
myWords [] = []
myWords s = [word] ++ myWords theRest
  where
    word = takeWhile (/= ' ') s
    theRest = dropWhile (== ' ') $ dropWhile (/= ' ') s
