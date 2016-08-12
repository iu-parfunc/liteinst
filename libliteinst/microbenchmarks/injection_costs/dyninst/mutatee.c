#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <stdlib.h>
#include <pthread.h>

#include "funcs.hpp"

#define tic() do { struct timespec ts_start, ts_end; clock_gettime(CLOCK_MONOTONIC, &ts_start)
#define toc() clock_gettime(CLOCK_MONOTONIC, &ts_end); \
              printf("%lfs\n", (ts_end.tv_sec - ts_start.tv_sec) + (double)(ts_end.tv_nsec - ts_start.tv_nsec)/1e9); } \
              while (0)

int n_funcs = 0;
int n_strides = 0;
int n_runners = 16;

void bar() {
  // printf("I am bar()\n");
  return;
}

int main(int argc, const char* argv[]) {
  printf("[mutatee] Hello World!\n");

  // n_funcs = atoi(argv[1]);
  n_runners = atoi(argv[1]);

  printf("[mutatee] Number of runners : %d\n", n_runners);

  // Thread fork
  pthread_t runners[n_runners]; 
  int rc;
  int *ids = new int[n_runners];

  for (int i=0; i < n_runners; i++) {
    ids[i] = i;
    int rc = pthread_create(&runners[i], NULL, runner, (void*)&ids[i]);
    if (rc) {
      printf("Error creating thread : %d\n", i);
      exit(-1);
    }
  }

  void* status;
  for (int i=0; i < n_runners; i++) {
    pthread_join(runners[i], &status);
  }

  return 0;
}
