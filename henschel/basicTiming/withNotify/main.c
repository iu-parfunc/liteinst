#include <stdio.h>
#include <time.h>


int myEmptyFunc( int i)
{
    return i+1;
}


int main()
{
    unsigned long int i,j;
    unsigned long int count=400000000;
    struct timespec start, stop;
    clock_gettime( CLOCK_REALTIME, &start);
    for (i=0; i < count; i++)
    {
#pragma noinline recursive
	j = myEmptyFunc(i);
    }
    clock_gettime( CLOCK_REALTIME, &stop);
    printf( "\n %2.15f seconds\n", (((stop.tv_sec*(double)1000000000)+stop.tv_nsec) - ((start.tv_sec*(double)1000000000)+start.tv_nsec))/(double)1000000000);
    printf( "\n %2.15f count\n", ((((stop.tv_sec*(double)1000000000)+stop.tv_nsec) - ((start.tv_sec*(double)1000000000)+start.tv_nsec))/(double)1000000000)/count);
    return j;
}