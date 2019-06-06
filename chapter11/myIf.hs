module MyIf where

data MyIf = If
data MyThen = Then
data MyElse = Else

myIf :: MyIf -> Bool -> MyThen -> a -> MyElse -> a -> a
myIf _ b _ x _ y
  | b         = x
  | otherwise = y



