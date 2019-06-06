module ZipListApplicative where
  
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)
  


instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  
instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _   = Nil
  _   <*> Nil = Nil
  (Cons f (Cons f' fs)) <*> (Cons x xs) = (Cons (f x) (fmap f xs)) `append` (Cons f' fs <*> Cons x xs)
  (Cons f Nil) <*> (Cons x xs) = (Cons (f x) (fmap f xs))