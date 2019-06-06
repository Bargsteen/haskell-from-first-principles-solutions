module Main where

import           Data.Monoid     (Monoid, mappend, mempty)
import           Data.Semigroup  (Semigroup, (<>))
import           Test.QuickCheck

-- Monoid Exercises
-- 1. Trivial

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity a = (a <> mempty) == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity a = (mempty <> a) == a


-- 2. Identity
newtype Identity a =
  Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity $ x <> y

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type IdentityStrAssoc =
  Identity String -> Identity String -> Identity String -> Bool


-- 3. Two
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b, Semigroup a, Semigroup b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoStrStrAssoc =
  Two String String -> Two String String -> Two String String -> Bool


-- 4. BoolConj
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  (BoolConj _)    <> (BoolConj _)    = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = frequency [ (2, return (BoolConj True))
                        , (1, return (BoolConj False))
                        ]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 4. BoolDisj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> (BoolDisj _)    = BoolDisj True
  (BoolDisj _)    <> (BoolDisj True) = BoolDisj True
  (BoolDisj _)    <> (BoolDisj _)    = BoolDisj False

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = frequency [ (2, return (BoolDisj True))
                        , (1, return (BoolDisj False))
                        ]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 6. Combine
newtype Combine a b =
  Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (f <> g)

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)

-- 7. Comp
newtype Comp a = Comp (a -> a)

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance (Semigroup a, Monoid a) => Monoid (Comp a) where
  mempty = Comp mempty
  mappend = (<>)

-- 8. Mem

newtype Mem s a =
  Mem { runMem :: s -> (a,s) }

instance Semigroup a => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) = Mem $ \s -> ((fst . f) s <> (fst . g) s, (snd . f . snd . g) s)

instance (Monoid a, Semigroup a) => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend = (<>)

f' :: Mem Integer String
f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mri = monoidRightIdentity
  putStrLn "\n-- Trivial --"
  quickCheck (sa :: TrivAssoc)
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mri :: Trivial -> Bool)
  putStrLn "\n-- Identity --"
  quickCheck (semigroupAssoc :: IdentityStrAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  putStrLn "\n-- Two --"
  quickCheck (semigroupAssoc :: TwoStrStrAssoc)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)
  putStrLn "\n-- BoolConj --"
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  putStrLn "\n-- BoolDisj --"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  putStrLn "\n-- Mem --"
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print rmleft
  print rmright
  print (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
