#include <time.h>

void set_seed(unsigned int);
unsigned int rand_int(void);

unsigned int seed;
const unsigned int a = 48271;
const unsigned int m = 0x7fffffff;

void set_seed(unsigned int s)
{
    seed = time(0); // initialize from current time of day
    seed = s;
}

unsigned int rand_int(void)
{
    return seed = (a * seed) % m;
}