
// Simple microbenchmark to measure latency across a software
// interrupt (SIGTRAP) and the OS bouncing back the signal to the
// user process.

#include <stdio.h>
#include <signal.h>
#include <memory.h>
#include "cycle.h"

ticks start, end;

// void catchit(int signo, siginfo_t * inf, void * ptr) { 
void catchit(int signo) { 
  end = getticks();
  double duration = elapsed(end, start);
  printf("Catching signal, num %d.. ticks elapsed %lf\n",signo, duration);
  printf("SELFTMED: %3.0lf\n", duration);
}

int main () {
  struct sigaction newact; 
  struct sigaction oldact; 
  memset( &newact, 0, sizeof newact);
  newact.sa_handler = & catchit;
  int i = sigemptyset(& (newact.sa_mask));
  // assert ( i == 0 );

  sigaction(SIGTRAP, &newact, &oldact);
  printf("Sigaction set, old funptr %p\n", oldact.sa_handler);
  start = getticks();
  end   = getticks();

  printf("Min elapsed %lf\n", elapsed(end,start));
  start = getticks();
  __asm__ ("int $0x03" : : );
  
  // printf("Exiting main function... should not see this?\n");
  return 0;
}
