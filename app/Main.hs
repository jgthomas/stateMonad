module Main where

import Stack
import StateM

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
  let res = evalState stackManip empty
  print res
