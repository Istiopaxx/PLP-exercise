#include <stdio.h>
#include <stdlib.h>
#include <math.h>

union
{
    volatile float f;
    volatile int i;
} a, b, c, d, e;

volatile float s;
volatile float t;

void test()
{
    a.f = 0.1;
    b.i = 0xbdcccccc;
    printf("a = %x %e\n", a.i, a.f);
    printf("b = %x %e\n", b.i, b.f);

    c.f = (float)(a.f + b.f);
    printf("a + b = %x %e\n", c.i, c.f);

    d.f = (float)(a.f * (float)10.0);
    e.f = (float)(b.f * (float)10.0);
    printf("a*10 = %x %e\n", d.i, d.f);
    printf("b*10 = %x %e\n", e.i, e.f);

    t = a.f + b.f;
    d.f = t * (float)10.0;
    /*
        d.f = (float) (a.f + b.f) * (float) 10.0;
    */
    printf("(a+b)*10 = %x %e\n", d.i, d.f);

    s = a.f * (float)10.0;
    t = b.f * (float)10.0;
    e.f = s + t;
    /*
        e.f = (float) (a.f * (float) 10.0) + (float) (b.f * (float) 10.0);
    */
    printf("(a*10) + (b*10) = %x %e\n\n", e.i, e.f);

    printf("difference = %f%% \n\n----------------\n", fabs(d.f - e.f) * 100 / e.f);
}

int main(void)
{
    printf("solution method: \n\n");
    test();

    printf("My method: \n\n");

    volatile float a = 0.1;
    volatile float b = -0.09999999;
    volatile float s, t, u, v;

    s = (float)(a + b);
    u = (float)(s * (float)10.0);
    printf("(0.1 + b) * 10.0 = %e\n", u);

    s = (float)(a * 10);
    t = (float)(b * 10);
    v = (float)(s + t);
    printf("(0.1 * 10.0) + (b * 10.0) = %e\n\n", v);

    printf("difference = %f%% \n", fabs(u - v) * 100 / v);

    return 0;
}
