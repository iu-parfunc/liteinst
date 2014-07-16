#include <stdio.h>
#include <time.h>
#include "zca-toggle.hpp"

int global_x;

static __inline__ unsigned long long getticks(void)
{
    unsigned long long int x;
    __asm__ volatile (".byte 0x0f, 0x31" : "=A" (x));
    return x;
}

void myEmptyFuncNotify( void )
{
    __notify_intrinsic((void*)"myEmptyFuncNotify:start", (void *)&global_x);
//    __notify_intrinsic((void*)"myEmptyFuncNotify:end", (void *)&global_x);
    return;
}

void myEmptyFunc( void )
{
    return;
}

int main()
{
    unsigned long int i;
    unsigned long int count=10000000;
    struct timespec start, stop;
    double cycleTime, hzRate;
    hzRate=(double)1866000000;
    unsigned long long tick1,tick2,cycles;

    printf("Initializing ZCAService...\n");
    initZCAService();
    printf("Done.\n");

    printf("Machine info:\n");
    cycleTime=(double)1/hzRate*(double)1000000000;
    printf( "  %3.5f GHz\n", hzRate/1000000000);
    printf( "  %3.10f cycle time in nano seconds\n", cycleTime);

    printf( "Calling an empty function:\n");
    clock_gettime( CLOCK_REALTIME, &start);
    tick1=getticks();
    for (i=0; i < count; i++)
    {
	#pragma noinline recursive
	myEmptyFunc();
    }
    clock_gettime( CLOCK_REALTIME, &stop);
    tick2=getticks();
    cycles = (unsigned long long)((tick2-tick1));
    printf( "  %3.10f runtime in seconds\n", (((stop.tv_sec*(double)1000000000)+stop.tv_nsec) - ((start.tv_sec*(double)1000000000)+start.tv_nsec))/(double)1000000000);
    printf( "  %3.10f runtime per function call in nano seconds\n", (((stop.tv_sec*(double)1000000000)+stop.tv_nsec) - ((start.tv_sec*(double)1000000000)+start.tv_nsec))/(double)count);
    printf( "  %3.10f cycles per function call (gettime)\n", ((((stop.tv_sec*(double)1000000000)+stop.tv_nsec) - ((start.tv_sec*(double)1000000000)+start.tv_nsec))/(double)count)/cycleTime);
    printf( "  %3.10f cycles per function call (RDTSC)\n", ((double)cycles)/((double)count));

    printf( "Calling an empty function with __notify_intrinsic:\n");
    clock_gettime( CLOCK_REALTIME, &start);
    tick1=getticks();
    for (i=0; i < count; i++)
    {
	#pragma noinline recursive
	myEmptyFuncNotify();
    }
    clock_gettime( CLOCK_REALTIME, &stop);
    tick2=getticks();
    cycles = (unsigned long long)((tick2-tick1));
    printf( "  %3.10f runtime in seconds\n", (((stop.tv_sec*(double)1000000000)+stop.tv_nsec) - ((start.tv_sec*(double)1000000000)+start.tv_nsec))/(double)1000000000);
    printf( "  %3.10f runtime per function call in nano seconds\n", (((stop.tv_sec*(double)1000000000)+stop.tv_nsec) - ((start.tv_sec*(double)1000000000)+start.tv_nsec))/(double)count);
    printf( "  %3.10f cycles per function call (gettime)\n", ((((stop.tv_sec*(double)1000000000)+stop.tv_nsec) - ((start.tv_sec*(double)1000000000)+start.tv_nsec))/(double)count)/cycleTime);
    printf( "  %3.10f cycles per function call (RDTSC)\n", ((double)cycles)/((double)count));
    return 0;
}