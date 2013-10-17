#include <stdio.h>
#include <time.h>


void myEmptyFunc( void )
{
    __notify_intrinsic((void*)"ENTER int myEmptyFunc() C", (void *)1);
    __notify_intrinsic((void*)"LEAVE int myEmptyFunc() C", (void *)1);
    return;
}

int main()
{
    unsigned long int i;
    unsigned long int count=1000000000;
    struct timespec start, stop;
    clock_gettime( CLOCK_REALTIME, &start);
    for (i=0; i < count; i++)
    {
	#pragma noinline
	myEmptyFunc();
    }
    clock_gettime( CLOCK_REALTIME, &stop);
    double cycleTime=(double)1/(double)1866000000*(double)1000000000;
    printf( "\n %2.15f cycle time in nano seconds\n", cycleTime);
    printf( "\n %2.15f runtime in seconds\n", (((stop.tv_sec*(double)1000000000)+stop.tv_nsec) - ((start.tv_sec*(double)1000000000)+start.tv_nsec))/(double)1000000000);
    printf( "\n %2.15f runtime per function call in nano seconds\n", (((stop.tv_sec*(double)1000000000)+stop.tv_nsec) - ((start.tv_sec*(double)1000000000)+start.tv_nsec))/(double)count);
    printf( "\n %2.15f cycles per function call\n", ((((stop.tv_sec*(double)1000000000)+stop.tv_nsec) - ((start.tv_sec*(double)1000000000)+start.tv_nsec))/(double)count)/cycleTime);
    return 0;
}