module Validation where
  
import Data.Monoid ((<>))
import Test.QuickCheck (Arbitrary, arbitrary, frequency, oneof)
import Test.QuickCheck.Checkers (quickBatch, EqProp, (=-=), eq)
import Test.QuickCheck.Classes
  
data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)
  
-- same as Either
instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)
  
-- This is different
instance Monoid e => Applicative (Validation e) where
  pure a = Success a
  (Failure e) <*> (Failure e') = Failure (e <> e')
  (Failure e) <*> _            = Failure e
  _           <*> (Failure e)  = Failure e
  (Success f) <*> (Success a)  = Success (f a)
  
instance (Eq e, Eq a) => EqProp (Validation e a) where (=-=) = eq

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    frequency [ (3, return (Success a))
              , (1, return (Failure e))]

data Errors =
   DividedByZero
 | StackOverflow
 | MooglesChewedWires deriving (Eq, Show)

instance Arbitrary Errors where
  arbitrary = oneof $ return <$> [DividedByZero, StackOverflow, MooglesChewedWires]

main :: IO ()
main = quickBatch (applicative (Success ("ab", 2 :: Int, 'a')))