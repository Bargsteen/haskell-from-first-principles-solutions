module Cap where

import Data.Char

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = rev . cap


fmapped :: String -> String
fmapped = rev <$> cap


tupled :: String -> (String, String)
tupled = (,) <$> composed <*> fmapped


tupled' :: String -> (String, String)
tupled' = do
  a <- rev
  b <- cap
  return (a, b)


tupled'' :: String -> (String, String)
tupled'' =
  rev >>=
  \r -> cap >>=
  \c -> return (r, c)
