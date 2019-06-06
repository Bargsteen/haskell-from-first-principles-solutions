module Tendigit where

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d     = xLast `mod` 10


tensDigit2 :: Integral a => a -> a
tensDigit2 x = d
    where xLast = fst $ divMod x 10
          d     = snd $ divMod xLast 10

hunsD :: Integral a => a -> a
hunsD x = d2
    where d  = fst $ divMod x 100
          d2 = snd $ divMod d 10
