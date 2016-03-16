
#include <stdio.h>
#include <time.h>

#include "messit.hpp"

void foo() {
  fprintf(stderr, "Inside foo..\n");
}

int main() {
  struct timespec t;
  t.tv_sec = 0;
  t.tv_nsec = 10000;
  for(int i=0; i<100; i++) {
    foo();
    clock_nanosleep(CLOCK_REALTIME, 0, &t, NULL);
  }
}
