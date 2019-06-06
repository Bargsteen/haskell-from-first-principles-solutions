module Main where

import           Data.List           (sort)
import           Test.QuickCheck
import           Text.Show.Functions


-- 1

half :: Fractional a => a -> a
half x = x / 2

halfIdentity = (*2) . half

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = x == halfIdentity x

-- 2

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t)      = (Just y, t)
          go y (Just x, t)       = (Just y, x >= y)

prop_listOrderedAfterSort :: [Int] -> Bool
prop_listOrderedAfterSort ls = listOrdered $ sort ls

-- 3

plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative x y =
  x + y == y + x

prop_plusIsAssociative :: Int -> Int -> Int -> Bool
prop_plusIsAssociative = plusAssociative

prop_plusIsCommutative :: Int -> Int -> Bool
prop_plusIsCommutative = plusCommutative

-- 4

multAssociative x y z =
  x * (y * z) == (x * y) * z

multCommutative x y =
  x * y == y * x

prop_multAssociative :: Int -> Int -> Int -> Bool
prop_multAssociative = multAssociative

prop_multCommutative :: Int -> Int -> Bool
prop_multCommutative = multCommutative

-- 5

prop_quotRemRelation :: Int -> Int -> Bool
prop_quotRemRelation x y = quot x y * y + rem x y == x

prop_divModRelation :: Int -> Int -> Bool
prop_divModRelation x y = div x y * y + mod x y == x

-- Results in Exception: divide by zero

-- 6
-- This will obviously fail

prop_powAssociative :: Int -> Int -> Int -> Bool
prop_powAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

prop_powCommutative :: Int -> Int -> Bool
prop_powCommutative x y = x ^ y == y ^ x

-- 7
prop_doubleReverseIsId :: [Int] -> Bool
prop_doubleReverseIsId xs = (reverse . reverse) xs == id xs

-- 8

prop_dollarDef :: (Int -> Int) -> (Int -> Int) -> Int -> Bool
prop_dollarDef f g x = dollar == compo
  where
    dollar = g $ f x
    compo = (g . f) x

-- 9
-- foldr (:) == (++)

--foldrConsDoublePlus :: String -> Char -> Bool
--foldrConsDoublePlus xs x = foldr (:) == (++)


-- foldr (++) [] == concat
foldrDoublePlusConcat :: Eq a => [[a]] -> Bool
foldrDoublePlusConcat xs = foldr (++) [] xs == concat xs

prop_foldrDoublePlusConcatString :: [String] -> Bool
prop_foldrDoublePlusConcatString = foldrDoublePlusConcat

-- 10
-- f n xs = length (take n xs) == n, fails obviously
takeLengthIsLength :: Int -> [a] -> Bool
takeLengthIsLength n xs = length (take n xs) == n

prop_takeLengthIsLengthInts :: Int -> [Int] -> Bool
prop_takeLengthIsLengthInts = takeLengthIsLength

-- 11
-- f x = (read (show x)) == x
readShowIsIdentity :: (Read a, Show a, Eq a) => a -> Bool
readShowIsIdentity x = (read . show) x == x

prop_readShowIsIdentityInt :: Int -> Bool
prop_readShowIsIdentityInt = readShowIsIdentity

main :: IO ()
main = do
  quickCheck prop_halfIdentity
  quickCheck prop_listOrderedAfterSort
  quickCheck prop_plusIsAssociative
  quickCheck prop_plusIsCommutative
  quickCheck prop_multAssociative
  quickCheck prop_multCommutative
  --quickCheck prop_quotRemRelation
  --quickCheck prop_divModRelation
  --quickCheck prop_powAssociative
  --quickCheck prop_powCommutative
  putStr "reverse "
  quickCheck prop_doubleReverseIsId
  putStr "$ "
  quickCheck prop_dollarDef
  putStr ": ++"
  quickCheck foldrConsDoublePlus
  putStr "++ concat"
  quickCheck prop_foldrDoublePlusConcatString
  putStr "take length"
  quickCheck prop_takeLengthIsLengthInts
  putStr "read . show = id"
  quickCheck prop_readShowIsIdentityInt
