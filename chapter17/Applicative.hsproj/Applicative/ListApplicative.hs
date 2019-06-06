module ListApplicative where

import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck          (Arbitrary, arbitrary, frequency)
import           Test.QuickCheck.Checkers (EqProp, eq, quickBatch, (=-=))
import           Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = (concat' . fmap f) as

toMyList :: Foldable t => t a -> List a
toMyList = foldr Cons Nil


instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)


-- (<$>) ::      (a -> b) -> List a -> List b
-- (<*>) :: List (a -> b) -> List a -> List b

instance Applicative List where
  pure x              = Cons x Nil
  _ <*> _             = Nil
  (Cons f fs) <*> xs  = (f <$> xs) `append` (fs <*> xs)


instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    l <- arbitrary
    frequency [ (1, return Nil)
              , (2, return (Cons a l))]

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let l = xs
                in take' 3000 l
          ys' = let l = ys
                in take' 3000l

-- Zip List

take' :: Int -> List a -> List a
take' 0 _           = Nil
take' _ Nil         = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)


newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs


instance Applicative ZipList' where
  pure x = ZipList' (Cons x Nil)
  (ZipList' Nil) <*> _ = ZipList' Nil
  _ <*> (ZipList' Nil) = (ZipList' Nil)
  (ZipList' (Cons f Nil)) <*> (ZipList' (Cons x _)) = ZipList' (Cons (f x) Nil)
  (ZipList' (Cons f _)) <*> (ZipList' (Cons x Nil)) = ZipList' (Cons (f x) Nil)
  (ZipList' (Cons f fs)) <*> (ZipList' (Cons x xs)) = ZipList' (Cons (f x) (fs <*> xs))


listSpec :: IO ()
listSpec = do
  quickBatch $ applicative (Cons ("a", "b", "c") Nil)




























