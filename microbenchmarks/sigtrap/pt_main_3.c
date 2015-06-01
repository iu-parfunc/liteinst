
// Details : Sigtrap signal handler with multliple threads hitting the interrupt.
// Result : Only interrupted threads get stopped and they resume after the
//          signal handler returns.

#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <memory.h>
#include <pthread.h>

// void catchit(int signo, siginfo_t * inf, void * ptr) { 
void catchit(int signo) { 
  printf("Catching signal, num %d.. Signal handler going to an infinite loop. \n",signo);
  int i;
  for(i=0; i< 5; i++) {
    sleep(1);
    printf("SIGHANDLE ..\n");
  }
}

void *interrupt(void *param) {
    long tid = (long)param;
    int i;
    for(i=0; i < 5; i++) {
      sleep(1);
      printf("%ld BEFORE INTERRUPT..\n", tid);
    }
    
    printf("%ld going to execute the trap interrupt\n", tid);
  __asm__ ("int $0x03" : : );

   // This should never get executed since the signal handler goes to an
   // infinite loop
    while(1) {
      sleep(1);
      printf("%ld AFTER INTERRUPT..\n", tid);
    }
}

void *regular(void *param) {
    long tid = (long)param;
    while(1) {
      sleep(1);
      printf("%ld REGULAR ..\n", tid);
    }
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
 
  int NUM_THREADS = 10;
  pthread_t threads[2*NUM_THREADS];
  int rc;

  for (i=0; i < 2*NUM_THREADS; i+=2) {
    int j=i;
    int k=i+1;

    rc = pthread_create(&threads[i], NULL, interrupt, (void*)j);
    if (rc) {
      printf("ERROR: thread creation failed with error %d\n", rc);
    }

    rc = pthread_create(&threads[i+1], NULL, regular, (void*)k);
    if (rc) {
      printf("ERROR: thread creation failed with error %d\n", rc);
    }
  }

  int *k = NULL;
  // Wait for all threads to finish
  for (i=0; i<2*NUM_THREADS; i++) {
    pthread_join(threads[i], (void**)&k);
  }
  
  return 0;
}
