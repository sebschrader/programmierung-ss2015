#include <stdio.h>

int main(){
  int x1, x2;
  scanf("%d", &x1);
  x2 = x1;
  while (x2 > 0){
    x1 = x1 * x2;
    x2 = x2 - 1;
  }
  printf("%d", x1);
  return 0;
}