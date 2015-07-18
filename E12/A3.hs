module Main where

f1   x1 = if x1 > 0  then f11 x1
                     else f2 x1
f11  x1 = if x1 > 0  then f111 x1
                     else f2 x1
f111 x1 = f11 (x1 - 1)
f2   x1 = x1

main = do x1 <- readLn
          print (f1 x1)
