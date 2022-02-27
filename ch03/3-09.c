#include <stdio.h>

int main(void)
{
    int a = 1, b = 2, c = 3;
    printf("%d %d %d\n", a, b, c);
    {
        int d = 4, e = 5;
        printf("%d %d\n", d, e);
        {
            int f = 7;
            printf("%d\n", f);
        }
    }
    {
        int g = 8, h = 9, i = 10;
        printf("%d %d %d\n", g, h, i);
    }

    return 0;
}