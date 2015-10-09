#include <stdio.h>

void myEmptyFunc()
{
}

int main()
{
    unsigned long int i;
#pragma noinline recursive
    for (i=0; i < 10000000; i++)
    {
	myEmptyFunc();
    }
}