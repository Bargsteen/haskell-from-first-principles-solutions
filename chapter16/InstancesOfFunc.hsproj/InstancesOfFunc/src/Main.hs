module Main where

import           Test.QuickCheck
import           Test.QuickCheck.Function

-- 16.10

-- Helpers
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                  (a -> b)
               -> (b -> c)
               -> f a
               -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) =>
                    f a
                -> Fun a b
                -> Fun b c
                -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type StringToString = Fun String String
type StringToInt = Fun String Int
type IntToString = Fun Int String

-- 1. Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

identityFI :: Identity Int -> Bool
identityFI = functorIdentity

type IdentityInt = Identity Int -> IntToInt -> IntToInt -> Bool

-- 2. Pair
data Pair a = Pair a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Pair x y)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

pairFI :: Pair Int -> Bool
pairFI = functorIdentity

type PairInt = Pair Int -> IntToInt -> IntToInt -> Bool

-- 3. Two
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

twoFI :: Two Int String -> Bool
twoFI = functorIdentity

type TwoIntString = Two Int String -> StringToInt -> IntToString -> Bool

-- 4. Three
data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

threeFI :: Three Int Bool String -> Bool
threeFI = functorIdentity

type ThreeIntBoolString = Three Int Bool String -> StringToString -> StringToInt -> Bool

-- 5. Three'
data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return (Three' a b b')

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

threeFI' :: Three' Int String -> Bool
threeFI' = functorIdentity

type ThreeIntString' = Three' Int String -> StringToInt -> IntToInt -> Bool

-- 6. Four
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return (Four a b c d)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

fourFI :: Four Int String Bool Int -> Bool
fourFI = functorIdentity

type FourIntStringBoolInt = Four Int String Bool Int -> IntToInt -> IntToString -> Bool

-- 7. Four'
data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a   <- arbitrary
    a'  <- arbitrary
    a'' <- arbitrary
    b   <- arbitrary
    return (Four' a a' a'' b)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

fourFI' :: Four' Int String -> Bool
fourFI' = functorIdentity

type FourIntString' = Four' Int String -> StringToString -> StringToInt -> Bool

-- 8. Trivial, can't because it has the kind *
data Trivial = Trivial


  
main :: IO ()
main = do
  putStrLn "Indentity:"
  quickCheck identityFI
  quickCheck (functorCompose' :: IdentityInt)
  putStrLn "Pair:"
  quickCheck pairFI
  quickCheck (functorCompose' :: PairInt)
  putStrLn "Two:"
  quickCheck twoFI
  quickCheck (functorCompose' :: TwoIntString)
  putStrLn "Three:"
  quickCheck threeFI
  quickCheck (functorCompose' :: ThreeIntBoolString)
  putStrLn "Three':"
  quickCheck threeFI'
  quickCheck (functorCompose' :: ThreeIntString')
  putStrLn "Four:"
  quickCheck fourFI
  quickCheck (functorCompose' :: FourIntStringBoolInt)
  putStrLn "Four':"
  quickCheck fourFI'
  quickCheck (functorCompose' :: FourIntString')



