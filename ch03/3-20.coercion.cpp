#include <iostream>

double min(double x, double y) {
    return x < y ? x : y;
}

int main(void) {
    std::cout << min(2147483647, 2147483646) << std::endl;
    std::cout << min(0.00001f, 0.022f) << std::endl;
    return 0;
}