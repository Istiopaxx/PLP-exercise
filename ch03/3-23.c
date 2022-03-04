#include <stdio.h>


#define GCD(A, B) (B == 0 ? A : GCD(B, A % B)) 

int main(void)
{
    int a = 4523, b = 112;
    printf("a = %d, b = %d, gcd = %d\n", a, b, GCD(a, b));
    return 0;
}


int gcd(int a, int b)
{
    int temp;
    while (b != 0)
    {
        temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}