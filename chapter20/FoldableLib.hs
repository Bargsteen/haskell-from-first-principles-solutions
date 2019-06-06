module FoldableLib where

import Data.Monoid
import Data.Semigroup
import Data.Foldable

-- Implement in terms of foldMap or foldr from Foldable

-- 1. sum
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum


-- 2. product
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product


-- 3. elem
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (\a -> Any $ a == x)


-- 4. minimum
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs | null xs = Nothing
           | otherwise = Just $ foldr min ((head . toList) xs) xs


-- 5. maximum
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = undefined

-- 6. null
null' :: (Foldable t) => t a -> Bool
null' = (0 /=) . length'

-- 7. length
length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (const (Sum 1))


-- 8. toList
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- 9. fold
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- 10. foldMap
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> f a `mappend` b) mempty

