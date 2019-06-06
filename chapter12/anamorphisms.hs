module Anamorphisms where

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)


myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f x =
  case (f x) of
    Just (a,b) -> a : myUnfoldr f b
    Nothing    -> []


betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\ b -> Just (b, f b)) x
