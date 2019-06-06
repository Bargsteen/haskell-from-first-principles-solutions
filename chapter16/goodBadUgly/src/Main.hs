module Main where

main :: IO ()
main = undefined


--(.) :: (b -> c) -> (a -> b) -> a -> c
--      fmap         fmap
--fmap :: Functor f => (m -> n) -> f m -> f n
--fmap :: Functor g => (x -> y) -> g x -> g y

--(fmap . fmap) :: (Functor g, Functor f) => (m -> n) -> g (f m) -> g (f n)
replaceWithP :: b -> Char
replaceWithP = const 'p'

ha :: Maybe [String]
ha = Just ["Ha", "Ha"]

lmls :: [Maybe [String]]
lmls = [ha, Nothing, Just []]

dblReplace :: [Maybe [String]] -> [Maybe Char]
dblReplace = (fmap . fmap) replaceWithP

tripleReplace :: [Maybe [String]] -> [Maybe String]
tripleReplace = (fmap . fmap . fmap) replaceWithP

quadReplace :: [Maybe [String]] -> [Maybe [String]]
quadReplace = (fmap . fmap . fmap . fmap) replaceWithP

dbl = dblReplace lmls
tlp = tripleReplace lmls
qd = quadReplace lmls
