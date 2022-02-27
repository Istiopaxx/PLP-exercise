#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    int a = 1;
    int b = 2;

    void middle(void)
    {
        int b = a;
        void inner(void)
        {
            printf("%d, %d\n", a, b);
        }
        int a = 3;

        inner();
        printf("%d, %d\n", a, b);
    }

    middle();
    printf("%d, %d\n", a, b);

    exit(0);
}