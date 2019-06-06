module EitherLib where

lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' (Right _:xs) = lefts' xs
lefts' (Left x:xs) = x : lefts' xs


lefts'' :: [Either a b] -> [a]
lefts'' = foldr (\ x acc -> go x acc) []
  where
    go (Left x) acc = x : acc
    go (Right _) acc = acc


rights' :: [Either a b] -> [b]
rights' = foldr (\ x acc -> go x acc) []
  where
    go (Left _) acc = acc
    go (Right x) acc = x : acc


partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' ls = (lefts'' ls, rights' ls)


eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just $ f x


either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ g (Right x) = g x


eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f e = either' (\_ -> Nothing) (\x -> Just $ f x) e


