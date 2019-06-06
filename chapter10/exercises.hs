module Exercises where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x then True else myOr xs

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False


--


myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if f x then True else myAny f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f ls = foldr (\ a b -> if f a then True else b) False ls

--

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y (x:xs) = if x == y then True else myElem y xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' e xs = foldr (\a b -> if a == e then True else b) False xs

--

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

-- ((([] f 1) f 2) f 3)

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myMap' :: (a -> b) -> [a] -> [b]
myMap' f xs = foldr (\a b -> f a : b) [] xs

myMap'' :: (a -> b) -> [a] -> [b]
myMap'' f = foldr ((:) . f) []

--

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs) = if f x then x : myFilter f xs else myFilter f xs

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' f ls = foldr (\a b -> if f a then a : b else b) [] ls

--

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squish' :: [[a]] -> [a]
squish' = foldr (++) []

--

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' f ls = foldr (\a b -> f a ++ b) [] ls

squishMap'' :: (a -> [b]) -> [a] -> [b]
squishMap'' f = foldr ((++) . f) []

--

squishAgain :: [[a]] -> [a]
squishAgain = squishMap' id

-- 

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:[]) = x
myMaximumBy f (x:xs) = if (f x other == GT) then x else other
  where
    other = myMaximumBy f xs

myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' f (x:xs) = foldr (\a b -> if f a b == GT then a else b) x xs


-- 

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldr (\a b -> if f a b == LT then a else b) x xs
