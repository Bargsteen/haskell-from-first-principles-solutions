module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid ((<>))
import Control.Monad (join, liftM2)


main :: IO ()
main = do
  putStrLn "Nope"
  quickBatch $ functor nopeTrigger
  quickBatch $ applicative nopeTrigger
  quickBatch $ monad nopeTrigger
  putStrLn "PhbtEither"
  quickBatch $ functor eitherTrigger
  quickBatch $ applicative eitherTrigger
  quickBatch $ monad eitherTrigger
  putStrLn "Identity"
  quickBatch $ functor identityTrigger
  quickBatch $ applicative identityTrigger
  quickBatch $ monad identityTrigger
  putStrLn "List"
  quickBatch $ functor listTrigger
  quickBatch $ applicative listTrigger
  quickBatch $ monad listTrigger



-- 1. Nope

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg


instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

nopeTrigger :: Nope (Int, String, Int)
nopeTrigger = undefined

-- 2. PhhhbbtttEither
data PhbtEither b a =
    Lefty a
  | Righty b
  deriving (Eq, Show)

instance Functor (PhbtEither b) where
  fmap f (Lefty a) = Lefty $ f a
  fmap _ (Righty b) = Righty b

instance Monoid b => Applicative (PhbtEither b) where
  pure = Lefty
  Righty b <*> Righty b' = Righty $ b <> b'
  Righty b <*> _ = Righty b
  _ <*> Righty b = Righty b
  Lefty f <*> Lefty a = Lefty $ f a

instance Monoid b => Monad (PhbtEither b) where
  return = pure
  Lefty a >>= f = f a
  Righty b >>= _ = Righty b

instance (Eq a, Eq b) => EqProp (PhbtEither b a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhbtEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (2, return $ Lefty a)
              , (1, return $ Righty b)]

eitherTrigger :: PhbtEither ([Int], String, [Int]) (String, Int, String)
eitherTrigger = undefined


-- 3. Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

identityTrigger :: Identity (Int, String, Bool)
identityTrigger = undefined


-- 4. List

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil Nil = Nil
  mappend Nil (Cons x xs) = Cons x xs
  mappend (Cons x xs) Nil = Cons x xs
  mappend (Cons x xs) other = Cons x (mappend xs other)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Applicative List where
  pure x = Cons x Nil
  Cons f fs <*> xs = (f <$> xs) <> (fs <*> xs)
  _ <*> _ = Nil

instance Monad List where
  return = pure
  Cons x xs >>= f = f x <> (xs >>= f)
  Nil >>= _ = Nil

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    l <- arbitrary
    frequency [ (1, return Nil)
              , (3, return $ Cons a l)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

listTrigger :: List (Int, String, Bool)
listTrigger = undefined


-- Part Two

-- 1. j

j :: Monad m => m (m a) -> m a
j = join

-- 2. l1
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f x = f <$> x

-- 3. l2
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2
-- l2 f ma mb = f <$> ma <*> mb

-- 4. a
a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf <*> ma

-- 5. meh
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:[]) f = return <$> f x
meh (x:xs) f = (return <$> f x) >>= (\r -> fmap (r ++) (meh xs f))


-- 6. flipType (reuse meh)
flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs id
