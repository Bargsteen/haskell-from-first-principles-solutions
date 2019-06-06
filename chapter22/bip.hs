module Bip where

import Control.Applicative

boop :: Integer -> Integer
boop = (*2)

doop :: Integer -> Integer
doop = (+10)

bip :: Integer -> Integer
-- bip = boop . doop
bip = fmap boop doop
-- fmap boop doop x == (*2) ((+10) x)


bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop


boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

