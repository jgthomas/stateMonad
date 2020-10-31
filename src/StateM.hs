module StateM where

import Control.Applicative (Applicative)
import Control.Monad (ap, liftM)

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  -- (Monad m) => a -> m a
  return a = State $ \s -> (a, s)

  -- (Monad m) => m a -> (a -> m b) -> m b
  --
  -- act = m a = (s -> (a, s))
  -- k = (a -> m b) = (s -> (s -> (a, s)))
  State act >>= k = State $ \s ->
    let (a, s') = act s -- pass initial state to (s -> (a, s))
     in runState (k a) s' -- apply k to return value to get (s -> (a, s))
    -- and pass this together with the new state, s',
    -- as the initial state to runState, which then
    -- returns the next (s -> (a, s))

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= \x -> put (f x)

evalState :: State s a -> s -> a
evalState act = fst . runState act

execState :: State s a -> s -> s
execState act = snd . runState act
