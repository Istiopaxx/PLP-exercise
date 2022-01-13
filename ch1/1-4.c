#include <stdio.h>

int main()
{
    int i, j;

    scanf("%d %d", &i, &j);
    while (1)
    {
        if (i == j)
        {
            printf("%d\n", i);
            break;
        }
        if (i > j)
            i = i - j;
        else
            j = j - i;
        printf("%d %d\n", i, j);
    }

    scanf("%d %d", &i, &j);
    while (1)
    {
        if (!i || !j)
        {
            printf("1\n");
            break;
        }
        if (i == j)
        {
            printf("%d\n", i);
            break;
        }
        if (i > j)
            i = i % j;
        else
            j = j % i;
        printf("%d %d\n", i, j);
    }

    return 0;
}