module Arithmetic where

import Test.QuickCheck
import Data.List (sort)

half :: Fractional a => a -> a
half x = x / 2

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = x == halfIdentity x

halfIdentity = (*2) . half

main :: IO ()
main = quickCheck prop_halfIdentity
