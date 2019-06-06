module Lookups where
  
import Data.List (elemIndex)

-- 1. added
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2. tupled
y2 :: Maybe Integer
y2 = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z2 :: Maybe Integer
z2 = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y2 <*> z2

-- 3. maxed
x3 :: Maybe Int
x3 = elemIndex 3 [1..5]

y3 :: Maybe Int
y3 = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x3 <*> y3

-- 4. summed
xs = [1, 2, 3]
ys = [4, 5, 6]

x4 :: Maybe Integer
x4 = lookup 3 $ zip xs ys

y4 :: Maybe Integer
y4 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x4 <*> y4