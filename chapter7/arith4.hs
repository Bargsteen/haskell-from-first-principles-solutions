module Arith4 where

roundTrip :: (Show a, Read b) => a -> b
roundTrip = (read . show)

main = do
  print ((roundTrip 4) :: String)
  print (id 4)
