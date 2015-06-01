
// Details : Sigtrap signal handler stalls for a while and then returns.
// Result : Only interrupted thread gets stopped and it continues after the
//          signal handler returns.

#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <memory.h>
#include <pthread.h>

// void catchit(int signo, siginfo_t * inf, void * ptr) { 
void catchit(int signo) { 
  printf("Catching signal, num %d.. Signal handler going to stall for a while. \n",signo);
  int i;
  for(i=0; i< 5; i++) {
    sleep(1);
    printf("SIGHANDLE ..\n");
  }
}

void *bar(void *param) {
    int i;
    for(i=0; i < 5; i++) {
      sleep(1);
      printf("BAR ..\n");
    }
    
    printf("Going to execute the trap interrupt\n");
  __asm__ ("int $0x03" : : );

   // This should never get executed since the signal handler goes to an
   // infinite loop
    while(1) {
      sleep(1);
      printf("BAR after the interrupt..\n");
    }
}

void *foo(void *param) {
    while(1) {
      sleep(1);
      printf("FOO ..\n");
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
 
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

  pthread_t foo_t;
  pthread_t bar_t;

  void *status;
  int rc;

  rc = pthread_create(&foo_t, &attr, foo, (void *)&foo);
  if (rc) {
    printf("ERROR; return code from pthread_create() is %d\n", rc);
    exit(1);
  }

  rc = pthread_create(&bar_t, &attr, bar, (void *)&bar);
  if (rc) {
    printf("ERROR; return code from pthread_create() is %d\n", rc);
    exit(1);
  }

  // Destroy the attribute and wait for other threads
  pthread_attr_destroy(&attr);
  rc = pthread_join(foo_t, &status);
  if (rc) {
    printf("ERROR; return code from pthread_join() is %d\n", rc);
    exit(-1);
  }

  rc = pthread_join(bar_t, &status);
  if (rc) {
    printf("ERROR; return code from pthread_join() is %d\n", rc);
    exit(-1);
  }
  
  // printf("Exiting main function... should not see this?\n");
  return 0;
}
