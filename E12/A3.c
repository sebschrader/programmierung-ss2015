#include <stdio.h>
int main() {
  int x1;
  scanf("%i", &x1);
  if (x1 > 0)
    while (x1 > 0) x1 = x1 - 1;
  printf("%d", x1);
  return 0;
}
