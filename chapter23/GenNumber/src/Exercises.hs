module Exercises where

import Moi

-- 1. Construct a State where the state is
-- also the value you return

get :: Moi s s
get = Moi (\s -> (s, s))


-- 2. Construct a State where the resulting state is the argument
-- provided and the value is defaulted to unit

put :: s -> Moi s ()
put s = Moi $ const ((), s)


-- 3. Run the State with s and get the state that results

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s


-- 4. Run the State with s and get the value that results.

eval :: Moi s a -> s -> a
eval (Moi sa) s = fst $ sa s


-- 5. Write a function which applies a function to create
-- a new State.

modify :: (s -> s) -> Moi s ()
modify f = Moi (\s -> ((), f s))
