module Main where

import StateM

type Stack = [Int]

empty :: Stack
empty = []

pop :: State Stack Int
pop = State $ \(x : xs) -> (x, xs)

push :: Int -> State Stack ()
push a = State $ \xs -> ((), a : xs)

tos :: State Stack Int
tos = State $ \(x : xs) -> (x, x : xs)

stackManip :: State Stack Int
stackManip = do
  push 10
  push 20
  a <- pop
  b <- pop
  push (a + b)
  tos

main :: IO ()
main = do
  let res = evalState stackManip Main.empty
  print res
