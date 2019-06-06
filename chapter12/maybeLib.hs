module MaybeLib where

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False


isNothing :: Maybe a -> Bool
isNothing = not . isJust


mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just x) = f x


fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just x) = x


listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x


maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]


catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just x):xs) = x : catMaybes xs


-- flipMaybe [Just 1, Just 2, Just 3] == Just [1,2,3]
-- flipMaybe [Just 1, Nothing, Just 2] == Nothing

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe ls = if any isNothing ls
               then Nothing
               else Just (catMaybes ls)














