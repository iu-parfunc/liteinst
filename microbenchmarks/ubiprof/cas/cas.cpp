
#include <stdio.h>
#include <inttypes.h>
#include <pthread.h>
#include <time.h>
#include <stdlib.h>
#include "../../../api/ubiprof.hpp"

volatile uint64_t ready_to_go = 0; // Interesting story here. Threads do not see globals in the intuitive sense
uint64_t foo_counter = 0;
void foo() {
  foo_counter++;
  return; 
}

volatile int i=0;

void* stress_cas(void* tid) {
  while(!ready_to_go){
    if (i++==0) {
      // printf("Looping with:%d\n", ready_to_go);
    }
  }
  for (int i=0; i<10000; i++) {
    PROFILER_INSTANCE->deactivateFunctionByName((void*)foo);
    PROFILER_INSTANCE->activateFunctionByName((void*)foo);
  }
  // printf("[CAS] Exiting thread : %d\n", (int)tid);
  return tid;
}

void* stress_invocation(void* tid) {
  while(!ready_to_go){
    ;
  }
  for (int i=0; i<10000; i++) {
    foo();
  }
  // printf("[Producer] Exiting thread : %d\n", (int)tid);
  return tid;
}

int main(int argc, char** argv) {
  
  // int NUM_THREADS = atoi(argv[1]);
  int NUM_THREADS = 20;
  pthread_t threads[2*NUM_THREADS];
  int rc;

  // Invoke it once to do the probe intialization with argument patching
  foo();
  
  for (int i=0; i < 2*NUM_THREADS; i+=2) {
    int j=i;
    int k=i+1;
    rc = pthread_create(&threads[i], NULL, stress_cas, (void*)j);
    if (rc) {
      printf("ERROR: thread creation failed with error %d\n", rc);
    }

    rc = pthread_create(&threads[i+1], NULL, stress_invocation, (void*)k);
    if (rc) {
      printf("ERROR: thread creation failed with error %d\n", rc);
    }
  }

  ready_to_go = 1;

  int *k = NULL;
  // Wait for all the therads finish
  for (int i=0; i<2*NUM_THREADS; i++) {
    pthread_join(threads[i], (void**)&k);
  }
  
  return 0;
}
