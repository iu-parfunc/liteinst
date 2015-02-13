#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

void foo() {

}


int main() {
  char* n_str = getenv("N");
  uint64_t n = 10000000000;
  if (n_str!= NULL) {
    n = atol(n_str);
  }

  // printf("N => %lu\n", n);
  //
// #pragma noinline recursive
  uint64_t result = 0;
  for (uint64_t i=0; i < n; i++) {
   result++; 
    // foo();
  } 
}



