{-# LANGUAGE RankNTypes #-}
module Intermission where
  

-- Exercise: Possibly
data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers a) = Yeppers (f a)
  fmap f LolNope     = LolNope

liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show


-- Exercise: Short Exercise

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a)  = First a
  fmap f (Second b) = Second $ f b
  

-- Wrapping
data Wrap f a = 
  Wrap (f a)
  deriving (Eq, Show)
  
instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)
  
-- Natural Transformations
type Nat f g = forall a . f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

