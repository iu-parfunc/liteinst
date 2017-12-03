
#include <stdio.h>
#include "cycle.h"

typedef int (*Fp)(void); 

int foo() {
  // printf("[FOO] Yay!!!\n");
  int x = 3433;
  x = x << 1;
  x++;
  return x;
}

int cb_handler(Fp fp) {
  return fp();
}

int main() {

  int x = 0;
  long n = 1000000000; 

  ticks direct = 0;
  ticks indirect = 0;
  ticks sanitized = 0;
  
  ticks start = getticks();
  for (long i = 0; i < n; i++) {
    foo();
  }
  ticks end = getticks();

  direct = end - start;

  Fp fp = foo;
  Fp* fpp = &fp;

  start = getticks();
  for (long i = 0; i < n; i++) {
    (*fp)();
  }

  end = getticks();

  indirect = end - start;

  /*
  start = getticks();
  for (long i = 0; i < n; i++) {
    (*fp)();
  }

  end = getticks();

  sanitized = end - start;
  */

  printf("Direct : %ld\n", direct);
  printf("Indirect : %ld\n", indirect);
  printf("Sanitized : %ld\n", sanitized);
  printf("Indirect - Direct : %ld\n", indirect - direct);
  // printf("Sanitized - Indirect: %ld\n", sanitized - indirect);
  printf("Direct cost : %ld\n", direct / n); 
  printf("Indirect cost : %ld\n", indirect / n); 
  // printf("Sanitized cost : %ld\n", sanitized / n); 

}
