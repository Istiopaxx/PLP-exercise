#include <stdio.h>
#include <limits.h>

int main() {
    int i;
    printf("limit on the size of integers: %d\n", INT_MAX);

    i = INT_MAX + 1;
    printf("arithmetic overflow <- INT_MAX + 1 : %d\n", i);
}