```
  trans(Max)
  
= trans(#include <stdio.h> int main(){... return 0;})

= blocktrans({int a, b, max; ... return 0;})

= stseqtrans(scanf("%i"); ... printf("%d", max); , update(int a, b, max;), 1)

= stseqtrans(scanf("%i"); ... printf("%d", max); , update(int a, b, max;, tab0), 1)

= stseqtrans(scanf("%i"); ... printf("%d", max); , tab1 = tab0[a/(var, 1), b/(var,2), max/(var,3)], 1)

= sttrans(scanf("%i", &a);, tab1, 1.1)
  sttrans(scanf("%i", &b);, tab1, 1.2)
  sttrans(if (a > b) max = a; else max = b;, tab1, 1.3)
  sttrans(printf("%d", max);, tab1, 1.4)

= READ 1;
  READ 2;
  boolexptrans(a>b, tab1)
  JMC 1.3.1;
  sttrans(max = b;, tab1, 1.3.2)
  JMP 1.3.3;
  sttrans(max = b;, tab1, 1.3.4) <- 1.3.1
  WRITE 3;

= READ 1;
  READ 2;
  LOAD 1;
  LOAD 2;
  GT;
  JMC 1.3.1;
  LOAD 1;
  STORE 3;
  JMP 1.3.3;
  LOAD 2;    <- 1.3.1
  STORE 3;
  WRITE 3;   <- 1.3.3
```
