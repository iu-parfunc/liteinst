

#include <stdio.h>
#include <cycle.h>

#include "profiler.hpp"

// TEMP HACK:
int global_x;

int main() {  
  start_profiler();

  __notify_intrinsic((void*)"main:start", (void *)&global_x);
  printf("Hello world\n");

  ticks start = getticks();
  while(getticks() - start < 1000 * 1000 * 1000)
  {
  }
  __notify_intrinsic((void*)"main:end", (void *)&global_x); 
  return 0;
}
