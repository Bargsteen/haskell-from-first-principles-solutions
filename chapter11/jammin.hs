module Jammin where

import Data.List

data Fruit =
      Peach
    | Plum
    | Apple
    | Blackberry
    deriving (Eq, Ord, Show)

data JamJars =
  Jam { fruit :: Fruit
          , jars   :: Int}
          deriving (Eq, Ord, Show)

row1 = Jam Plum 4
row2 = Jam Apple 20
row3 = Jam Blackberry 2
row4 = Jam Apple 2
row5 = Jam Peach 4
row6 = Jam Peach 5
row7 = Jam Apple 1

allJam = [row1, row2, row3, row4, row5, row6, row7]

jamjars :: [JamJars] -> [Int]
jamjars = map jars

totalJars :: [JamJars] -> Int
totalJars = foldr ((+) . jars) 0

compareJars (Jam _ n) (Jam _ m) = compare n m
compareKind (Jam k _) (Jam l _) = compare k l
jamEq (Jam k _) (Jam l _) = k == l


mostRow :: [JamJars] -> JamJars
mostRow ls = head $ sortBy compareJars ls

sortedJams :: [JamJars] -> [JamJars]
sortedJams = sortBy compareKind

groupJam :: [JamJars] -> [[JamJars]]
groupJam = map (sortBy compareJars) . groupBy jamEq . sortedJams

