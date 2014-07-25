
/* 
 * Measure the cost of measuring stack-depth at runtime, as a function
 * of stack depth.  It should be (and is) linear.
 */

#include <execinfo.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include "cycle.h"

void** stack;
static int size = 256;

int foo(int n) {
  if (n == 0) {
    int i; ticks st, end;
    // printf("  Calling backtrace...\n");
    st  = getticks();
    int depth = backtrace(stack, size);
    end = getticks();
    if(0) {
      printf("  At stack depth %d:\n", depth);   
      printf("  Getting names:\n");
      char** names = backtrace_symbols(stack, size);
      for(i=0; i<depth; i++)
        puts(names[i]);
    }
    printf("  %d: %lf\n", depth, elapsed(end,st));
    return depth;
  } else {
    // printf("  foo going deeper... %d\n", n);
    int res = foo(n-1);
    // printf("  foo returned to %d frame \n", n);
    return res * (res-5);
  }
}

int main () {
  stack = (void**)malloc(sizeof(void*) * size);
  printf("Initialized array to %p\n", stack);

  printf("Ignore this first one, warmup:\n");
  foo(1);

  printf("Stack_depth: backtrace_cycles\n");
  int i;
  for(i=0; i<200; i++) foo(i);
  printf("Done.\n");
}
