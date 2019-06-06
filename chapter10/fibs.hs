module Fibs where

fibs = 1 : scanl (+) 1 fibs

fibs' = take 20 fibs

fibs'' = filter (< 100) fibs

factorial = scanl (*) 1 [2..] 
