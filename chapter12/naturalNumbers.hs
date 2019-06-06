module NaturalNumbers where

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = succ (natToInteger n)

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | i == 0 = Just Zero
  | otherwise = Just (toNat i)
  where
    toNat 0 = Zero
    toNat n = Succ (toNat $ pred n)
