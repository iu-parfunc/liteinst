
#include <stdio.h>
#include <inttypes.h>
#include <assert.h>
#include <stdlib.h>

typedef unsigned long long ticks;

__attribute__((no_instrument_function))
static __inline__ ticks getticks(void) {
  unsigned a, d; 
  asm volatile("rdtsc" : "=a" (a), "=d" (d)); 
  return ((ticks)a) | (((ticks)d) << 32); 
}

__attribute__ ((noinline))
void foo(int x ,int y) {

}

__attribute__ ((noinline))
void bar() {

}

int main(int argc, char** argv) {

  // call them once to deactivate instrumentation
  foo(434, 24); 
  bar();

  uint64_t N = 1000000000; 
  N = atoi(argv[1]);
  int i;
  int64_t foo_total = 0;
  int64_t bar_total = 0;

  // To warm up stuff
  /*
  for (i = 0; i < N; i++) {
    foo(234, 454);
    bar();
  }
  */

// #ifdef INDIVIDUAL
  ticks start = 0, end = 0;
  for (i = 0; i < N; i++) {
    start = getticks();
    foo(234, 454);
    end = getticks();

    if (end < start) {
        printf("start : %llu  end : %llu", start, end);
    }
    assert(end > start);
    foo_total += (end - start);
  } 

  for (i = 0; i < N; i++) {
    start = getticks();
    bar();
    end = getticks();
    if (end < start) {
        printf("start : %llu  end : %llu\n", start, end);
    }
    assert(end > start);
    bar_total += (end - start);
  } 

  fprintf(stdout, "Total invocations : %lu\n", N);
  fprintf(stdout, "[regular] foo overhead : %ld\n", (foo_total/ N));
  fprintf(stdout, "[regular] bar overhead : %ld\n", (bar_total/ N));
// #endif

// #ifdef AGGREGATE 
  start = getticks();
  for (i = 0; i < N; i++) {
    foo(234, 454);
  } 
  end = getticks();
  foo_total = 0;
  foo_total = (end - start);

  start = getticks();
  for (i = 0; i < N; i++) {
    bar();
  } 
  end = getticks();
  bar_total = 0;
  bar_total = (end - start);

  fprintf(stdout, "[Loop] foo overhead : %ld\n", (foo_total/ N));
  fprintf(stdout, "[Loop] bar overhead : %ld\n", (bar_total/ N));
// #endif

}
