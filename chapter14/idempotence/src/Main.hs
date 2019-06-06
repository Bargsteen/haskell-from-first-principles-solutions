module Main where

import           Data.Char       (toUpper)
import           Data.List       (sort)
import           Test.QuickCheck

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs
capitalizeWord ""     = ""


f :: String -> Bool
f x = (capitalizeWord x == twice capitalizeWord x) && (capitalizeWord x == fourTimes capitalizeWord x)

f' :: Ord a => [a] -> Bool
f' x = (sort x == twice sort x) && (sort x == fourTimes sort x)



main :: IO ()
main = do
  putStr "Idempotence Tests\nCapitalizeWord: "
  quickCheck f
  putStr "Sort: "
  quickCheck (f' :: [Int] -> Bool)
