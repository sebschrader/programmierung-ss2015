module Main where

f::Int -> Int -> Int -> Int
f x1 x2 x3 = if x1 == 1 
              then x2
              else f (x1 - 1) x3 ((x2 * x2) + (x3 * x3))

main = do x1 <- readLn x1
          print (f x1 1 1)