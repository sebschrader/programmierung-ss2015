#include <stdio.h>

int main() {
  int a, b, sum;
  sum = 0;
  scanf("%i", &a);
  if (a < 0)
    a = 0;
  while (a > 0) {
    scanf("%i", &b);
    sum = sum + b;
    a = a - 1;
  }
  printf("%d", sum);
  return 0;
}
