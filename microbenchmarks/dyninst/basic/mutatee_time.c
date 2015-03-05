#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include "cycle.h"

#define tic() do { struct timespec ts_start, ts_end; clock_gettime(CLOCK_MONOTONIC, &ts_start)
#define toc() clock_gettime(CLOCK_MONOTONIC, &ts_end); \
              printf("%lfs\n", (ts_end.tv_sec - ts_start.tv_sec) + (double)(ts_end.tv_nsec - ts_start.tv_nsec)/1e9); } \
              while (0)

int N = 10000000;

void foo() {
    // sleep(1);
    // printf("I am foo()\n");
    return;
}

void bar() {
    return;
}

int main()
{

    ticks start = getticks();
    for (int i=0; i< N; i++){
      foo();
    }
    ticks end = getticks();

    fprintf(stderr, "CYCLES PER CALL : %lu\n", (end-start) / N);

    return 0;
}
