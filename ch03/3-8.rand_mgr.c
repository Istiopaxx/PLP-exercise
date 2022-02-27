#include <time.h>
#include <stdlib.h>

const unsigned int a = 48271;
const unsigned int m = 0x7fffffff;

typedef struct
{
    unsigned int seed;
} generator;

generator *create()
{
    generator *g = malloc(sizeof(generator));
    g->seed = time(0);
    return g;
}

void set_seed(generator *g, unsigned int s)
{
    g->seed = s;
}

unsigned int rand_int(generator *g)
{
    return g->seed = (a * g->seed) % m;
}
