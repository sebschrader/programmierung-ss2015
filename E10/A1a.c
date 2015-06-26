#include <stdio.h>
int main() {
    int x, y;
    scanf("%i", &x);
    y = 0;
    while (x > 0) {
        x = x - 1;
        y = y + 1;
    }
    printf("%d", y);
    return 0;
}
