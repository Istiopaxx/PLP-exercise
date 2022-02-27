#include <iostream>

int min(int a, int b) {
    return a < b ? a : b;
}

float min(float a, float b) {
    return a < b ? a : b;
}


int main(void) {
    printf("%d\n", min(2147483647, 2147483646));
    printf("%f\n", min(0.00001f, 0.022f));

    return 0;
}

