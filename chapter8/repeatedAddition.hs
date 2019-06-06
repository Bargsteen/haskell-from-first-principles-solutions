module RepeatedAddition where

repAddMultiply :: Integral a => a -> a -> a
repAddMultiply x y = go 0 x y 
  where go sum z n
         | n < 0 = go sum (-z) (-n)
         | n > 0 = go (sum + z) z (n-1)
         | n == 0 = sum
