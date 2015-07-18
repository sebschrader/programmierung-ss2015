Gegeben dieses H0 Programm.
```
module Main where

f :: Int -> Int -> Int
f x1 x2 = if x2 > 0 
            then f (x1*x2) (x2-1) 
            else g x1      (x1*x1)

g :: Int -> Int -> Int
g x1 x2 = if x1 < 100 
            then x2 
            else x1

main = do x1 <- readLn
          print (f x1 (2*x1))

```
Wird zu diesem C0 Programm transformiert.

```
#include <stdio.h>

int main() {
  int x1, x2, function, flag, result;
  scanf("%d", &x1)
  x2 = (2*x1);
  function = 1;
  flag = 1;
  while (flag == 1) {
    if (function == 1)
      if (x2 > 0) {
        x1 = x1 * x2;
        x2 = x2 - 1;
      }
      else {
        x2 = x1 * x1;
        function = 2;
      }
    else if (function == 2)
      if (x1 < 100) {
        result = x2;
        flag = 0;
      }
      else{
        result = x1
        flag = 0;
      }
  }
  printf("%d", result);
  return 0;
}

```