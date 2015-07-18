Gegeben dieses H0 Programm.
```
module Main where

h :: Int -> Int -> Int
h x1 x2 = if x2 == x1 then 30
                      else x2
g :: Int -> Int -> Int
g x1 x2 = if 10 <= x2 then g (x1-x2) (x2-1)
                      else h (x1+x2) 10

main = do x1 <- readLn
          print (g (3+x1) 5)
```
Wird zu diesem C0 Programm transformiert.

```
#include <stdio.h>

int main() {
  int x1, x2, function = 2, flag, result;
  scanf("%d",&x1);
  x1 = 3 + x1;
  x2 = 5;
  flag = 1;
  while (flag == 1) {
    if (function == 1)
      if (x2 == x1) {
        result = 30
        flag = 0;
      }
      else {
        result = x2;
        flag = 0;
      }
    else if (function == 2) {
      if(10 <= x2){
        x1 = x1 + x2;
        x2 = x2 - 1;
      }
      else{
        x1 = (x1 + x2)
        x2 = 10;
        function = 1;
      }
    }
  }
  printf("%d", result);
  return 0;
}
```