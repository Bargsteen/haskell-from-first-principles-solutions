{-# LANGUAGE FlexibleInstances #-}

module Exercises where
  
-- Rearrange type constructors to make the Functor instance work

-- 1.

data Sum b a = -- from Sum a b
    First a 
  | Second b
  
instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b
  

-- 2.
data Company a c b = 
    DeepBlue a c 
  | Something b
  
instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c
  
-- 3.
data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)
  
instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'
  

-- Write Functor instances for the following datatypes

-- 1. Quant

data Quant a b = 
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)
  
instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)
  

-- 2. K

data K a b =
  K a
  deriving (Eq, Show)
  

instance Functor (K a) where
  fmap _ (K a) = K a


-- 3. K Flip

newtype Flip f a b = 
  Flip (f b a)
  deriving (Eq, Show)
  
instance Functor (Flip K a) where 
  fmap f (Flip (K a)) = Flip $ K (f a)
  
fk :: K Int Char; fk = K 3
tryItOut = fmap (+1) (Flip fk)

-- 4. EvilGoateeConst

data EvilGoateeConst a b =
  GoatyConst b
  deriving (Eq, Show)
  
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)
  
-- 5. LiftItOut

data LiftItOut f a = 
  LiftItOut (f a)
  deriving (Eq, Show)
  
instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- 6. Parappa

data Parappa f g a = 
  DaWrappa (f a) (g a)
  deriving (Eq, Show)
  
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)


-- 7. IgnoreOne
data IgnoreOne f g a b = 
  IgnoringSomething (f a) (g b)
  deriving (Eq, Show)
  
instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)


-- 8. Notorious

data Notorious g o a t =
  Notorious (g o) (g a) (g t)
  deriving (Eq, Show)
  
instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)
  
-- 9. List
data List a = 
    Nil
  | Cons a (List a)
  deriving (Eq, Show)
  
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  

-- 10. GoatLord

data GoatLord a = 
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)
  
instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats ga ga' ga'') = MoreGoats (fmap f ga) (fmap f ga') (fmap f ga'')
  

  
-- 11. TalkToMe
data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a) 
  

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g) = Read (fmap f g)










