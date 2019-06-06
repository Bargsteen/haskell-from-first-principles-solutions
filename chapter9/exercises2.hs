module Exercises2 where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x then x else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if f x then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y (x:xs) = if y == x then True else myElem y xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x list = myAny (== x) list

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy _ [x] = x
myMaximumBy f (x:xs)
    | res == LT = second
    | res == GT = first
    | res == EQ = first
    where
        first = x
        second = myMaximumBy f xs
        res = f first second


myMaximum :: (Ord a) => [a] -> a
myMaximum l = myMaximumBy compare l
