
#include <stdio.h>
#include <inttypes.h>
#include <assert.h>
#include <stdlib.h>
#include "cycle.h"

#define ticks uint64_t

__attribute__ ((no_instrument_function))
static __inline__ unsigned long long getticks_1(void)
{
    unsigned long long int x;
    __asm__ volatile (".byte 0x0f, 0x31" : "=A" (x));
    return x;
}

__attribute__ ((noinline))
void bar() {

}

int main(int argc, char** argv) {

  long long N = 1000000000; 
  N = atoi(argv[1]);
  int i;
  long long bar_total = 0;

  // To warm up stuff
  /*
  for (i = 0; i < N; i++) {
    bar();
  }
  */

  ticks start = 0, end = 0;

  for (i = 0; i < N; i++) {
    start = getticks();
    bar();
    end = getticks();
    if (end < start) {
        printf("start : %lu  end : %lu diff : %lld\n", start, end , ((long long int)end - start));
    }
    assert(end > start);
    bar_total += (end - start);
  } 

  fprintf(stdout, "Total invocations : %lld\n", N);
  fprintf(stdout, "[regular] bar overhead : %lf\n", ((double)bar_total/ N));

  start = getticks();
  for (i = 0; i < N; i++) {
    bar();
  } 
  end = getticks();

  if (end < start) {
    printf("start : %lu  end : %lu diff : %lld\n", start, end, ((long long int)end - start));
  }
  assert(end > start);

  bar_total = (end - start);

  fprintf(stdout, "[Loop] bar overhead : %lf\n", ((double)bar_total/ N));

}
