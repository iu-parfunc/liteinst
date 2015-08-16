#include <stdio.h>
#include <unistd.h>
#include <time.h>

#define tic() do { struct timespec ts_start, ts_end; clock_gettime(CLOCK_MONOTONIC, &ts_start)
#define toc() clock_gettime(CLOCK_MONOTONIC, &ts_end); \
              printf("%lfs\n", (ts_end.tv_sec - ts_start.tv_sec) + (double)(ts_end.tv_nsec - ts_start.tv_nsec)/1e9); } \
              while (0)

void foo() {
    // sleep(1);
    printf("I am foo()\n");
    return;
}

void bar() {
    printf("I am bar()\n");
    return;
}

int main()
{
    printf("Hello World from mutatee!\n");

    //    tic();
    for (int i = 0; i < 1; ++i)
        foo();
    //    toc();

    return 0;
}
