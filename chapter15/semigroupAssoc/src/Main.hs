{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Data.Semigroup
import           GHC.Generics
import           Test.QuickCheck

------------------------
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool
------------------------
------------------------
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup (Identity a) where
  Identity x <> _ = Identity x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type IdenIntAssoc =
  Identity Int -> Identity Int -> Identity Int -> Bool
------------------------
------------------------
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two x y <> Two x' y' = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoStringStringAssoc =
  Two String String -> Two String String -> Two String String -> Bool
------------------------
------------------------
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c)
        => Semigroup (Three a b c) where
  Three x y z <> Three x' y' z' = Three (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeStringAssoc =
     Three String String String
  -> Three String String String
  -> Three String String String
  -> Bool
------------------------
------------------------
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  BoolConj _ <> BoolConj _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary =
    frequency [(2, return (BoolConj True)),
               (1, return (BoolConj False))]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool


f x y = x + y
------------------------
------------------------
newtype BoolDisj =
  BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj True <> _ = BoolDisj True
  _ <> BoolDisj True = BoolDisj True
  _ <> _ = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = frequency [ (1, return (BoolDisj True))
                        , (1, return (BoolDisj False))]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

------------------------
------------------------
data Or a b =
    Fst a
  | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Snd x <> _ = Snd x
  Fst _ <> Snd y = Snd y
  Fst _ <> Fst y = Fst y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (2, return (Fst a))
              , (3, return (Snd b))]

type OrStringStringAssoc = Or String String -> Or String String -> Or String String -> Bool
------------------------
------------------------
newtype Combine a b =
  Combine {unCombine :: a -> b} deriving (Generic)

instance Show (Combine a b) where
  show _ = "<<combine func>>"



instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \x -> f x <> g x

{-trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary-}

{-genFunc :: (CoArbitrary a, Arbitrary b) => Gen (a -> b)
genFunc = arbitrary

genCombine :: (CoArbitrary a, Arbitrary b) => Gen (Combine a b)
genCombine = do
  f <- genFunc
  return $ Combine f

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = genCombine-}

type CombineIntegerSumAssoc =
     Combine Integer (Sum Integer)
  -> Combine Integer (Sum Integer)
  -> Combine Integer (Sum Integer)
  -> Bool

------------------------
------------------------
-- Semigruop  Ex 10
newtype Comp a =
  Comp {unComp :: a -> a}

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp $ g . f

------------------------
------------------------
-- Semigroup Ex 11

data Validation a b =
  Failure' a | Success' b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Success' x) <> _ = Success' x
  (Failure' _) <> (Success' x) = Success' x
  (Failure' x) <> (Failure' y) = Failure' $ x <> y


instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (2, return (Failure' a))
              , (3, return (Success' b))]

------------------------
------------------------
semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
  putStrLn "Trivial"
  quickCheck (semigroupAssoc :: TrivAssoc)
  putStrLn "Identity Int"
  quickCheck (semigroupAssoc :: IdenIntAssoc)
  putStrLn "Two String String"
  quickCheck (semigroupAssoc :: TwoStringStringAssoc)
  putStrLn "Three String String String"
  quickCheck (semigroupAssoc :: ThreeStringAssoc)
  putStrLn "BoolConj"
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  putStrLn "BoolDisj"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  --putStrLn "Or String String"
  --quickCheck (semigroupAssoc :: OrStringStringAssoc)
  --putStrLn "Combine Integer (Sum Integer)"
  --quickCheck (semigroupAssoc :: CombineIntegerSumAssoc)



