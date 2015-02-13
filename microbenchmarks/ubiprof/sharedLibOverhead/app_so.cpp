#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "cycle.h"

void foo() {

}

int main() {
  char* n_str = getenv("N");
  // uint64_t n = 100000000;
  uint64_t n = 316519501;
  if (n_str!= NULL) {
    n = atol(n_str);
  }

  // printf("N => %lu\n", n);

  ticks elapsed = 0;
#pragma noinline recursive
  for (uint64_t i=0; i < n; i++) {
    // ticks start = getticks();
    foo();
    // ticks end = getticks();
    // elapsed += (end - start);
  } 
  // ticks elapsed = end - start;

  // fprintf(stderr, "Cycles per function call : %lu\n", elapsed / n);
  
}



