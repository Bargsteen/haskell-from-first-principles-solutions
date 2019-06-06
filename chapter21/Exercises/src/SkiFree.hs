{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a )
        => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance ( Applicative n
         , Testable (n Property)
         , EqProp a )
        => EqProp (S n a) where
  (S x y) =-= (S p q) = property ((=-=) <$> x <*> p) .&. (y =-= q)


instance Functor n => Functor (S n) where
  fmap f (S n a) = S (f <$> n) (f a)

instance Applicative n => Applicative (S n) where
  pure x = S (pure x) x
  S fs f <*> S xs x = S (fs <*> xs) (f x)

instance Foldable n => Foldable (S n) where
  foldMap f (S xs x) = foldMap f xs <> f x

instance Traversable n => Traversable (S n) where
  --traverse :: (Traversable n, Applicative f) => (a -> f b) -> S n a -> f (S n b)
  traverse f (S na a) = S <$> sequenceA (f <$> na) <*> f a


main =
  sample' (arbitrary :: Gen (S [] Int))
