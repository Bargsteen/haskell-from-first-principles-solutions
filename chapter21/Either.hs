module Either where

data Either' a b =
    Lefty a
  | Righty b
  deriving (Eq, Ord, Show)

instance Functor (Either' a) where
  fmap _ (Lefty a) = Lefty a
  fmap f (Righty b) = Righty $ f b


instance Applicative (Either' e) where
  pure = Righty
  Lefty e <*> _ = Lefty e
  Righty f <*> r = fmap f r


instance Foldable (Either' a) where
  foldMap _ (Lefty _) = mempty
  foldMap f (Righty y) = f y

  foldr _ z (Lefty _) = z
  foldr f z (Righty y) = f y z


instance Traversable (Either' a) where
  traverse _ (Lefty x) = pure (Lefty x)
  traverse f (Righty y) = Righty <$> f y

  sequenceA (Lefty fx) = Lefty <$> pure fx
  sequenceA (Righty fy) = Righty <$> fy



