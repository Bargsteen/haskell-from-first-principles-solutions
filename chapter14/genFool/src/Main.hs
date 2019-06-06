module Main where

import           Test.QuickCheck

data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

instance Arbitrary Fool where
  arbitrary = oneof [return Fulse, return Frue]

data Fool' =
    Fulse'
  | Frue'
  deriving (Eq, Show)

instance Arbitrary Fool' where
  arbitrary = frequency [ (2, return Fulse')
                        , (1, return Frue')]

main :: IO ()
main = undefined
