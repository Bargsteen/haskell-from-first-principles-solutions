module DividedBy where

data DividedResult =
    Result Integer
    | DividedByZero

dividedBy :: Integer -> Integer -> (DividedResult, DividedResult) 
dividedBy num denom 
    | denom == 0 = (DividedByZero, DividedByZero)
    | otherwise = go num denom 0
  where go n   d count
         | n < d = (Result count, Result n)
         | otherwise = go (n - d) d (count + 1)
