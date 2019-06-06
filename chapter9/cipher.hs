module Cipher where

import Data.Char

caesar :: Integer -> String -> String
caesar _ [] = []
caesar k (x:xs) = (if encOrd > (ord 'z') then moddedEnc else enc) : caesar k xs
    where 
      enc = chr encOrd
      moddedEnc = (chr . (+) (ord 'a') . (flip mod) (ord 'z' + 1)) encOrd

caesar k (x:xs) = (if withinAandZOrd encOrd then enc else moddedEnc) : caesar k xs
    where 
      encOrd = ord x + k
      enc = chr encOrd
      moddedEnc = 


adjustToWithinBounds :: Char -> Char
adjustToWithinBounds x
    | ordX < ordA = ordA - ordX 

withinAandZOrd :: Integer -> Bool
withinAAndZOrd x 
    | x <= (ord 'z') && x >= (ord 'a') = True
    | otherwise = False

