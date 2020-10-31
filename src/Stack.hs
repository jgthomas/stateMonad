module Stack where

import StateM (State(State))

type Stack = [Int]

empty :: Stack
empty = []

pop :: State Stack Int
pop = State $ \(x : xs) -> (x, xs)

push :: Int -> State Stack ()
push a = State $ \xs -> ((), a : xs)

tos :: State Stack Int
tos = State $ \(x : xs) -> (x, x : xs)
