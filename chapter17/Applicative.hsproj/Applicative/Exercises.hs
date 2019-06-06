module Exercises where
  
import Control.Applicative
import Data.Monoid (Sum, (<>))
import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers (quickBatch, EqProp, (=-=), eq)
import Test.QuickCheck.Classes

sumInt :: Sum Int
sumInt = mempty

main :: IO ()
main = do 
  quickBatch (applicative (Pair ('a', "as", 'a') ('a', "as" , 'a')))
  quickBatch (applicative (Two sumInt ("a", "as" , "aa")))
  quickBatch (applicative 
    (Three sumInt sumInt (sumInt, sumInt, sumInt)))
  quickBatch (applicative (Three' sumInt (sumInt, "a", sumInt) (sumInt, "a", sumInt)))
  quickBatch (applicative 
    (Four sumInt sumInt "a" (sumInt, sumInt, sumInt)))
  quickBatch (applicative 
    (Four' sumInt sumInt sumInt (sumInt, sumInt, sumInt)))



-- 1. Pair
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (Pair f f') <*> (Pair a a') = Pair (f a) (f' a')
  
instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return (Pair a a')
    
instance (Eq a) => EqProp (Pair a) where (=-=) = eq


-- 2. Two
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
  
instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two a f) (Two a' b) = Two (a <> a') (f b)
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)
  
instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

-- 3. Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
  
instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (Three a b f) <*> (Three a' b' c) = Three (a <> a') (b <> b') (f c)
  
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)
    
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

-- 4. Three'
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  (Three' a f g) <*> (Three' a' b b') = Three' (a <> a') (f b) (g b')
  
instance (Arbitrary a, Arbitrary b) 
  => Arbitrary (Three' a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      b' <- arbitrary
      return (Three' a b b')
      
instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq


-- 5. Four 
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)
  
instance (Monoid a, Monoid b, Monoid c) 
  => Applicative (Four a b c) where
    pure d = Four mempty mempty mempty d
    (Four a b c f) <*> (Four a' b' c' d) 
      = Four (a <> a') (b <> b') (c <> c') (f d)
      
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return (Four a b c d)
      
instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) 
  where (=-=) = eq
  

-- 7. Four'

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)
  
instance Monoid a => Applicative (Four' a) where
  pure b = Four' a a a b where a = mempty
  (Four' a1 a2 a3 f) <*> (Four' a1' a2' a3' b) 
    = Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    b <- arbitrary
    return (Four' a1 a2 a3 b)
    
instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq
























