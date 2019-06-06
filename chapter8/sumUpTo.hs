module SumUpTo where

sumUpTo :: (Eq a, Num a) => a -> a
sumUpTo n = go n 0
  where go m sum
         | m == 0 = sum
         | m /= 0 = go (m - 1) (sum + m)
