module Trees where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x =
  case (f x) of
    Just (l, b, r) -> Node (unfold f l) b (unfold f r)
    Nothing        -> Leaf


treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold go 0
  where go x
          | x < n = Just (x + 1, x, x + 1)
          | otherwise = Nothing
