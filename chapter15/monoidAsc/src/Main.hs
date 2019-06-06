module Main where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m)
            => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo)]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool


--

data Optional a =
  Nada
    | Only a
    deriving (Eq, Show)

newtype First' a =
  First' {getFirst' :: Optional a}
  deriving (Eq, Show)

firstGen :: Arbitrary a => Gen (First' a)
firstGen = do
  a <- arbitrary
  frequency [ (3, return (First' (Only a)))
            , (1, return (First' Nada))]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = firstGen

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' (Only x)) _ = First' (Only x)
  mappend (First' Nada) (First' (Only x)) = First' (Only x)
  mappend (First' Nada) (First' Nada) = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId = First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

  {-let ma = monoidAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (ma :: BullMappend)
  quickCheck (mli :: Bull -> Bool)
  quickCheck (mlr :: Bull -> Bool)-}

