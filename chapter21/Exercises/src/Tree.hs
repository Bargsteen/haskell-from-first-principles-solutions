module Tree where

data Tree a =
   Empty
 | Leaf a
 | Node (Tree a) a (Tree a)
 deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node left a right) = Node (f <$> left) (f a) (f <$> right)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node left a right) = foldMap f left <> f a <> foldMap f right

  foldr _ b Empty = b
  foldr f b (Leaf a) = f a b
  foldr f b (Node left a right) = f a (foldr f (foldr f b right) left)


instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node left a right) = Node <$> traverse f left <*> f a <*> traverse f right
