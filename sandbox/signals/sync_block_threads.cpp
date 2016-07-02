
#include <assert.h>
#include <signal.h>
#include <pthread.h>
#include <iostream>
#include <cstring>

/* Test for blocking synchronous signals within multiple threads.
 * Behavior is
 * 1. If raise is used handler is not run and threads continue from the 
 *    interrupted location (this is unsafe)
 * 2. If int3 is used handler is not run but program fails with a trap 
 *    potentially due to same interrupt being issued twice.
 *    http://stackoverflow.com/questions/29759129/why-does-ignoring-sigtrap-not-work-with-asm
 */


using std::cout;

void* trap(void* arg) {
  cout << "Entering thread..\n";
  
  sigset_t mask;
  sigset_t orig_mask;
  sigemptyset (&mask);
  sigaddset (&mask, SIGTRAP);

  if (pthread_sigmask(SIG_BLOCK, &mask, &orig_mask) < 0) {
    perror ("sigmask pthread error");
    return NULL;
  }

  // raise(SIGTRAP);
  asm("int3");
  cout << "Leaving thread..\n";
}

void trap_handler(int signo, siginfo_t* inf, void* ptr) {
  cout << "IN_TRAP\n"; // unsafe
}

int main() {

  struct sigaction g_newact;
  struct sigaction g_oldact;

  memset( &g_newact, 0, sizeof g_newact);
  g_newact.sa_sigaction = & trap_handler;
  g_newact.sa_flags = SA_SIGINFO|SA_NODEFER;
  sigemptyset(& (g_newact.sa_mask));

  sigaction(SIGTRAP, &g_newact, &g_oldact);

  cout << "Spawning threads..\n";

  pthread_t th[2];
  int is_spawned = 0;
  for (int i=0; i < 2; i++) {
    is_spawned |=  pthread_create(&th[i], NULL, trap, NULL);
  }

  assert(!is_spawned);

  for (int i=0; i < 2; i++) {
    pthread_join(th[i], NULL);
  }

  return 0;

}
