module McCarthy where

mc91 :: (Num a, Ord a) => a -> a
mc91 x
  | x > 100 = x - 10
  | x <= 100 = mc91 $ mc91 $ x + 11
