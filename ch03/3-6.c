#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    int g;

    void B(int a)
    {
        int x;

        void A(int n)
        {
            g = n;
        }

        void R(int m)
        {
            printf("x: %d in R and m is %d\n", x, m);
            x /= 2;
            if (x > 1)
                R(m + 1);
            else
                A(m);
        }

        x = a * a;
        R(1);
    }

    B(3);
    printf("g: %d in main\n", g);

    exit(0);
}