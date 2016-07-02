
#include <assert.h>
#include <signal.h>
#include <pthread.h>
#include <iostream>
#include <cstring>

/* Test for blocking synchronous signals
 * Behavior is
 * 1. If raise is used handler is not run and program continues from the 
 *    interrupted location (this is unsafe)
 * 2. If int3 is used handler is not run but program fails with a trap 
 *    potentially due to same interrupt being issued twice.
 *    http://stackoverflow.com/questions/29759129/why-does-ignoring-sigtrap-not-work-with-asm
 */

using std::cout;

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

  sigset_t mask;
  sigset_t orig_mask;
  sigemptyset (&mask);
  sigaddset (&mask, SIGTRAP);

  if (pthread_sigmask(SIG_BLOCK, &mask, &orig_mask) < 0) {
    perror ("sigmask pthread error");
    return 1;
  }

  raise(SIGTRAP);
  raise(SIGTRAP);
  // asm("int3");
  
  cout << "Continuing with main..\n";

  /*
  if (pthread_sigmask(SIG_SETMASK, &orig_mask, NULL) < 0) {
    perror ("sigmask pthread error");
    return 1;
  }
  */

  return 0;

}
