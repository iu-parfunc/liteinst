#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include "papi.h"

int test(int i) {
//__notify_intrinsic((void*)"ENTER int myEmptyFunc() C", (void *)1);
    return i+1;
}

void papiInit(void) {
   if (PAPI_num_counters() < 2) {
	fprintf(stderr, "No hardware counters here, or PAPI not supported.\n");
	exit(1);
    }
}

void papiStart(int events[2]) {
    int ret;
    if ((ret = PAPI_start_counters(events, 2)) != PAPI_OK) {
	fprintf(stderr, "PAPI failed to start counters: %s\n", PAPI_strerror(ret));
	exit(1);
    }
}

void papiStop(long long *values[2]) {
    int ret;
    if ((ret = PAPI_read_counters(values, 2)) != PAPI_OK) {
	fprintf(stderr, "PAPI failed to read counters: %s\n", PAPI_strerror(ret));
	exit(1);
    }
}

int main (void ) {
    int events[2] = {PAPI_L1_DCM, PAPI_TOT_CYC },i=0;
    long long values[2];
    long long j=0;

    papiInit();

    papiStart(events);

    for (i = 0; i<10000000000; i++) {
	j=test(j);
    }

    papiStop(&values[0]);


    printf("Cycles = %lld\n",values[1]);
    printf("Time = %f seconds\n",(double)values[1]*((double)1/(double)2500000000));
    printf("L2 data cache misses =  %lld\n\n", values[0]);
    printf("Count: %lld\n",j);
    
    return(0);
}

