module Main where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

type TI = []

main :: IO ()
main = do
  putStrLn "id"
  quickBatch (traversable idTrigger)
  putStrLn "const"
  quickBatch (traversable constTrigger)
  putStrLn "optional"
  quickBatch (traversable optionalTrigger)
  putStrLn "List"
  quickBatch (traversable listTrigger)
  putStrLn "three"
  quickBatch (traversable threeTrigger)
  putStrLn "pair"
  quickBatch (traversable pairTrigger)
  putStrLn "big"
  quickBatch (traversable bigTrigger)
  putStrLn "bigger"
  quickBatch $ traversable biggerTrigger

-- Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Foldable Identity where
  foldr f b (Identity a) = f a b
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a
  sequenceA (Identity a) = fmap pure a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

idTrigger :: Identity (Int, Int, [Int])
idTrigger = undefined

-- Constant
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  x <*> x' = Constant $ getConstant x `mappend` getConstant x'

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  sequenceA c = Constant <$> pure (getConstant c)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq

constTrigger :: Constant String (Int, Int, [Int])
constTrigger = undefined


-- Maybe
data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap f (Yep x) = Yep (f x)
  fmap _ _ = Nada

instance Applicative Optional where
  pure = Yep
  Yep f <*> Yep x = Yep $ f x
  _ <*> _ = Nada

instance Foldable Optional where
  foldMap f (Yep a) = f a
  foldMap _ _ = mempty

instance Traversable Optional where
  traverse f (Yep x) = Yep <$> f x
  traverse _ _ = pure Nada

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [ (2, Yep <$> arbitrary)
                        , (1, pure Nada )
                        ]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

optionalTrigger :: Optional (Int, Int, [Int])
optionalTrigger = undefined


-- List

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap _ Nil = Nil

instance Semigroup (List a) where
  xs <> Nil = xs
  Nil <> xs = xs
  Cons x Nil <> Cons x' Nil = Cons x (Cons x' Nil)
  Cons x xs <> ys = Cons x (xs <> ys)

instance Applicative List where
  pure x = Cons x Nil
  Cons f fs <*> xs = (f <$> xs) <> (fs <*> xs)
  Nil <*> _ = Nil

instance Foldable List where
  foldMap f (Cons x xs) = f x <> foldMap f xs
  foldMap _ Nil = mempty

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (3, return $ Cons a b)
              , (1, return Nil)]

instance Eq a => EqProp (List a) where (=-=) = eq

listTrigger :: List (Int, String, String)
listTrigger = undefined

-- Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  Three a b f <*> Three a' b' x = Three (a <> a') (b <> b') (f x)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

threeTrigger :: Three String String (Int, Int, [Int])
threeTrigger = undefined


-- Pair
data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Monoid a => Applicative (Pair a) where
  pure = Pair mempty
  Pair a f <*> Pair a' x = Pair (a <> a') (f x)

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

instance (Eq a, Eq b) => EqProp (Pair a b) where (=-=) = eq

pairTrigger :: Pair String (Int, String, String)
pairTrigger = undefined


-- Big
data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Monoid a => Applicative (Big a) where
  pure x = Big mempty x x
  Big a f f' <*> Big a' x x' = Big (a <> a') (f x) (f' x')

instance Foldable (Big a) where
  foldMap f (Big _ b b') = f b <> f b'

instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    pure (Big a b b')

instance (Eq a, Eq b) => EqProp (Big a b) where (=-=) = eq

bigTrigger :: Big String (String, Int, String)
bigTrigger = undefined


-- traverse  :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
-- (<*>)     :: (Applicative f) => f (a -> b) -> f a -> f b

-- Bigger
data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b1 b2 b3) = Bigger a (f b1) (f b2) (f b3)

instance Monoid a => Applicative (Bigger a) where
  pure x = Bigger mempty x x x
  Bigger a f1 f2 f3 <*> Bigger a' b1 b2 b3 = Bigger (a <> a') (f1 b1) (f2 b2) (f3 b3)

instance Foldable (Bigger a) where
  foldr f c (Bigger _ b1 b2 b3) = f b1 $ f b2 $ f b3 c
  foldMap f (Bigger _ b1 b2 b3) = f b1 <> f b2 <> f b3

instance Traversable (Bigger a) where
  traverse f (Bigger a b1 b2 b3) = Bigger a <$> f b1 <*> f b2 <*> f b3

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    b3 <- arbitrary
    return (Bigger a b1 b2 b3)

instance (Eq a, Eq b) => EqProp (Bigger a b) where (=-=) = eq

biggerTrigger :: Bigger String (String, Char, [Int])
biggerTrigger = undefined
