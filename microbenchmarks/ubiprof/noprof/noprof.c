
#include <stdio.h>
#include <inttypes.h>
#include <assert.h>
#include <stdlib.h>

#define ticks uint64_t

__attribute__ ((no_instrument_function))
static __inline__ unsigned long long getticks(void)
{
    unsigned long long int x;
    __asm__ volatile (".byte 0x0f, 0x31" : "=A" (x));
    return x;
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

  long long N = 1000000000; 
  N = atoi(argv[1]);
  int i;
  long long foo_total = 0;
  long long bar_total = 0;

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
        printf("start : %lld  end : %lld\n");
    }
    assert(end > start);
    foo_total += (end - start);
  } 

  for (i = 0; i < N; i++) {
    start = getticks();
    bar();
    end = getticks();
    if (end < start) {
        printf("start : %lld  end : %lld\n");
    }
    assert(end > start);
    bar_total += (end - start);
  } 

  fprintf(stdout, "Total invocations : %lld\n", N);
  fprintf(stdout, "[regular] foo overhead : %lld\n", (foo_total/ N));
  fprintf(stdout, "[regular] bar overhead : %lld\n", (bar_total/ N));
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

  fprintf(stdout, "[Loop] foo overhead : %lld\n", (foo_total/ N));
  fprintf(stdout, "[Loop] bar overhead : %lld\n", (bar_total/ N));
// #endif

}
