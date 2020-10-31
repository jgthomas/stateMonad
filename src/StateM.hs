-- longer version of >>=
--
-- State act >>= k = \s ->
--   let (a, s') = act s
--       (State act') = k a
--       in
--     act' s' -- (s -> (a, s)) applied to an initial state, s'
--
-- >>= returns a function \s -> (a, s)
-- takes in act, (s -> (a, s))
-- and k, (a -> (s -> (a, s)))
-- and builds the two functions, 'act' and 'k'
-- into the returned \s -> (a, s) using a
-- closure-like mechanism, such that the final returned
-- function can be given an initial s, and run through
-- all the enclosed steps, with runState then returning the
-- final (a, s) if passed an s
module StateM where

import Control.Applicative (Applicative)
import Control.Monad (ap, liftM)

-- constructor of the state monad holds a function
-- that takes an initial state, and returns
-- a pair of a return value and a new state
-- runState is the convenience accessor function
newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  -- (Monad m) => a -> m a
  -- a -> (s -> (a, s))
  return a = State $ \s -> (a, s)

  -- (Monad m) => m a -> (a -> m b) -> m b
  -- (s -> (a, s)) -> (a -> (s -> (b, s))) -> (s -> (b, s))
  --
  -- two arguments to >>=, act and k
  -- act = m a = (s -> (a, s))
  -- k = (a -> m b) = (a -> (s -> (a, s)))
  State act >>= k = State $ \s ->
    let (a, s') = act s -- pass initial state to (s -> (a, s))
     in runState (k a) s' -- apply k to return value to get (s -> (a, s))
    -- and pass this together with the new state, s',
    -- as the initial state to runState, which then
    -- returns the next (s -> (a, s))

-- returns a State constructor in which the
-- function has the current state (s -> (s, s)) as the return value
get :: State s s
get = State $ \s -> (s, s)

-- takes a state, s, then returns a State constructor in which the
-- function has that state, s, as its state
put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= \x -> put (f x)

-- fst (a, s) is the return value, a
evalState :: State s a -> s -> a
evalState act = fst . runState act

-- snd (a, s) is the state, s
execState :: State s a -> s -> s
execState act = snd . runState act
