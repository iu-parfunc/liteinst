#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "dynaprof.h"

int global_x;

static __inline__ unsigned long long getticks(void)
{
    unsigned long long int x;
    __asm__ volatile (".byte 0x0f, 0x31" : "=A" (x));
    return x;
}

void myEmptyFunc0( void ) {
 __notify_intrinsic((void*)"myEmptyFunc0:start", (void *)&global_x);
 __notify_intrinsic((void*)"myEmptyFunc0:end", (void *)&global_x);
return; }
void myEmptyFunc1( void ) {
 __notify_intrinsic((void*)"myEmptyFunc1:start", (void *)&global_x);
 __notify_intrinsic((void*)"myEmptyFunc1:end", (void *)&global_x);
return; }
void myEmptyFunc2( void ) {
 __notify_intrinsic((void*)"myEmptyFunc2:start", (void *)&global_x);
 __notify_intrinsic((void*)"myEmptyFunc2:end", (void *)&global_x);
return; }
void myEmptyFunc3( void ) {
 __notify_intrinsic((void*)"myEmptyFunc3:start", (void *)&global_x);
 __notify_intrinsic((void*)"myEmptyFunc3:end", (void *)&global_x);
return; }
void myEmptyFunc4( void ) {
 __notify_intrinsic((void*)"myEmptyFunc4:start", (void *)&global_x);
 __notify_intrinsic((void*)"myEmptyFunc4:end", (void *)&global_x);
return; }
void myEmptyFunc5( void ) {
 __notify_intrinsic((void*)"myEmptyFunc5:start", (void *)&global_x);
 __notify_intrinsic((void*)"myEmptyFunc5:end", (void *)&global_x);
return; }
void myEmptyFunc6( void ) {
 __notify_intrinsic((void*)"myEmptyFunc6:start", (void *)&global_x);
 __notify_intrinsic((void*)"myEmptyFunc6:end", (void *)&global_x);
return; }
void myEmptyFunc7( void ) {
 __notify_intrinsic((void*)"myEmptyFunc7:start", (void *)&global_x);
 __notify_intrinsic((void*)"myEmptyFunc7:end", (void *)&global_x);
return; }
void myEmptyFunc8( void ) {
 __notify_intrinsic((void*)"myEmptyFunc8:start", (void *)&global_x);
 __notify_intrinsic((void*)"myEmptyFunc8:end", (void *)&global_x);
return; }
void myEmptyFunc9( void ) {
 __notify_intrinsic((void*)"myEmptyFunc9:start", (void *)&global_x);
 __notify_intrinsic((void*)"myEmptyFunc9:end", (void *)&global_x);
return; }
unsigned long int countTEST=10;

int main(int argc, char** argv)
{
    unsigned long int i;
    struct timespec start, stop;
    double cycleTime, hzRate;
    hzRate=(double)1866000000;
    unsigned long long tick1,tick2,cycles;

    if (argc <= 3) {
      printf("Error: needs 3 command line arguments: F, N, M.  See README.md.");
      return 1;
    }

    int NUMFUNS  = atoi(argv[1]);
    int NUMCALLS = atoi(argv[1]);
    int MEMTRAFF = atoi(argv[2]);
    
    printf("Initializing DynaProf... sampleOverhead benchmark: F=%d, N=%d, M=%d\n", NUMFUNS, NUMCALLS, MEMTRAFF);
    printf("  F = NUMFUNS  \n");
    printf("  N = NUMCALLS (per function) \n");
    printf("  M = MEMORYTRAFFIC, in words between calls\n");

    int* garbage = (int*)malloc(sizeof(int) * MEMTRAFF);
    int junk_acc = 0;
    start_profiler();
    printf("Initialization finished, now start proper benchmark.\n");

    clock_gettime( CLOCK_REALTIME, &start);
    tick1=getticks();
    for (int i=0; i<NUMCALLS; i++)
      for (int j=0; j<NUMFUNS; j++) {
	// Call the probed function:
	myEmptyFunc0();
	// Trash that cache:
	for (int k=0; k<MEMTRAFF; k++) junk_acc += garbage[k];
      }
    start_profiler();
    clock_gettime( CLOCK_REALTIME, &stop);
    tick2=getticks();
    printf("Done.\n");

    printf("Machine info:\n");
    cycleTime=(double)1/hzRate*(double)1000000000;
    printf( "  %3.5f GHz\n", hzRate/1000000000);
    printf( "  %3.10f cycle time in nano seconds\n", cycleTime);

    cycles = (unsigned long long)((tick2-tick1));
    double cycles_per = ((double)cycles) / (double)NUMCALLS / (double)NUMFUNS;
    printf( "  %3.10f runtime in seconds\n", 
           (((stop.tv_sec*(double)1000000000)+stop.tv_nsec) - 
	    ((start.tv_sec*(double)1000000000)+start.tv_nsec))/(double)1000000000);
    printf( "  %3.10f cycles per function sample (RDTSC)\n", cycles_per);
    printf( "SELFTIMED: %3.0f\n", ((double)cycles_per));
    return 0;
}
