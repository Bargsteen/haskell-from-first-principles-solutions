module FoldableExercises where

import Data.Monoid

-- Write Foldable instances for the following datatypes

-- 1.
data Constant a b = Constant b

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b
  foldr f c (Constant b) = f b c

-- 2. Two
data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b
  foldr f c (Two _ b) = f b c


-- 3. Three a b c
data Three a b c = Three a b c deriving (Show, Eq)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c
  foldr f d (Three _ _ c) = f c d

-- 4. Three'
data Three' a b = Three' a b b deriving (Show, Eq)

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = mappend (f b) (f b')
  foldr f c (Three' _ b b') = b `f` (b' `f` c)


-- 5. Four
data Four a b = Four a b b b

instance Foldable (Four a) where
  foldMap f (Four _ b1 b2 b3) = f b1 <> f b2 <> f b3
  foldr f c (Four _ b1 b2 b3) = b1 `f` (b2 `f` (b3 `f` c))


-- filterF, write a filter function for Foldable using foldMap

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)

-- use is:
-- xs = [1..10]
-- res :: Sum Int; res = filterF even xs
