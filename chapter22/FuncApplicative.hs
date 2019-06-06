{-# LANGUAGE InstanceSigs #-}
module FuncApplicative where

import Control.Applicative (liftA2)

newtype Reader r a = Reader {runReader :: r -> a}


newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName   :: DogName
  , address   :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName    :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")

kasper :: Person
kasper =
  Person (HumanName "Kasper Bargsteen")
         (DogName "Jasmin")
         (Address "Danmarksgade")

-- without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

(<$->>) :: (a -> b) -> (r -> a) -> (r -> b)
(<$->>) = (<$>)

(<*->>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
(<*->>) = (<*>)

getDogR' :: Person -> Dog
getDogR' = Dog <$->> dogName <*->> address
-- getDogR' = (\dName addr -> Dog dName addr) <$->> (\pers -> dogName pers) <*->> (\pers -> address pers)

-- with Reader, alternative
getDogR'' :: Person -> Dog
getDogR'' = liftA2 Dog dogName address
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

-- with Reader Monad
getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy


-- ** Exercise: Reading Comprehension **

-- 1. Write liftA2
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 g fa fb = g <$> fa <*> fb


-- 2. Write the following function
asks :: (r -> a) -> Reader r a
asks = Reader


instance Functor (Reader r) where
  fmap f (Reader r) = Reader (f . r)


-- 3. Applicative Reader instance
instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  Reader rab <*> Reader ra = Reader $ \r -> rab r (ra r)


instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  Reader ra >>= aRrb = Reader $ \r -> (runReader $ aRrb (ra r)) r


getDogRM' :: Reader Person Dog
getDogRM' = Reader (Dog <$> dogName <*> address)
