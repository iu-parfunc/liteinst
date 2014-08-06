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
typedef void (*voidfun)(void);
voidfun funtable[10];
void init_funtable() {
  funtable[0] = & myEmptyFunc0; 
  funtable[1] = & myEmptyFunc1; 
  funtable[2] = & myEmptyFunc2; 
  funtable[3] = & myEmptyFunc3; 
  funtable[4] = & myEmptyFunc4; 
  funtable[5] = & myEmptyFunc5; 
  funtable[6] = & myEmptyFunc6; 
  funtable[7] = & myEmptyFunc7; 
  funtable[8] = & myEmptyFunc8; 
  funtable[9] = & myEmptyFunc9; 
}



int* garbage; // Global

unsigned long long run_all(int NUMFUNS, int MEMTRAFF) {
    int junk_acc = 0;
    unsigned long long tick1, tick2, time_acc = 0;
    
    // Calling a large # of different functions also trashes the cache:
    for (int j=0; j<NUMFUNS; j++) {
      // Call the probed function:
      // myEmptyFunc0(); // FINISHME!! Need an array of function pointers.
      tick1=getticks();
      (funtable[j])();
      tick2=getticks();
      time_acc += (tick2 - tick1);
      // Trash that cache:
      for (int k=0; k<MEMTRAFF; k++) junk_acc += garbage[k];
    }

    if (junk_acc == 999999)      
      return time_acc;
    else       
      return time_acc;
}

int main(int argc, char** argv)
{
    unsigned long int i;
    struct timespec start, stop;
    unsigned long long cycles = 0;

    if (argc <= 3) {
      printf("Error: needs 3 command line arguments: F, N, M.  See README.md.");
      return 1;
    }

    int NUMFUNS  = atoi(argv[1]);
    int NUMCALLS = atoi(argv[2]);
    int MEMTRAFF = atoi(argv[3]);
    
    printf("Initializing DynaProf... sampleOverhead benchmark: F=%d, N=%d, M=%d\n", NUMFUNS, NUMCALLS, MEMTRAFF);
    printf("  F = NUMFUNS  \n");
    printf("  N = NUMCALLS (per function) \n");
    printf("  M = MEMORYTRAFFIC, in words between calls\n");

    garbage = (int*)malloc(sizeof(int) * MEMTRAFF);
    start_profiler();
    init_funtable();

    printf("Initialization finished, now start proper benchmark.\n");
    // Warm up the code cache before timing:
    for(int i=0; i<100; i++)
      run_all(NUMFUNS, 0);
    run_all(NUMFUNS, MEMTRAFF);
    
    // clock_gettime( CLOCK_REALTIME, &start);
    for (int i=0; i<NUMCALLS; i++)
      cycles += run_all(NUMFUNS, MEMTRAFF);
    // clock_gettime( CLOCK_REALTIME, &stop);
    printf("Done.\n");

    double cycles_per = ((double)cycles) / ((double)NUMCALLS * (double)NUMFUNS);
    printf( "  %3.10f runtime in seconds\n", 
           (((stop.tv_sec*(double)1000000000)+stop.tv_nsec) - 
	    ((start.tv_sec*(double)1000000000)+start.tv_nsec))/(double)1000000000);
    printf( "  %d / (%d * %d) = %3.0f cycles per function sample (RDTSC)\n", 
	    cycles,NUMCALLS,NUMFUNS, cycles_per);
    printf( "SELFTIMED: %3.0f\n", ((double)cycles_per));
    return 0;
}
