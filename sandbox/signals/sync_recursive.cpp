
#include <signal.h>
#include <pthread.h>
#include <iostream>
#include <cstring>

/* Test for invoking synchronous signals recursively.
 * Behavior is
 *   1. If SA_NODEFER is not specified signal is blocked and the program faults
 *      with an unhandled trap.
 *   2. If SA_NODEFER is specified the trap handler is entered recursively.
 */

using std::cout;

void trap_handler(int signo, siginfo_t* inf, void* ptr) {
  cout << "IN_TRAP\n"; // unsafe
  asm("int3");
}

int main() {

  struct sigaction g_newact;
  struct sigaction g_oldact;

  memset( &g_newact, 0, sizeof g_newact);
  g_newact.sa_sigaction = & trap_handler;
  g_newact.sa_flags = SA_SIGINFO;
  sigemptyset(& (g_newact.sa_mask));

  sigaction(SIGTRAP, &g_newact, &g_oldact);

  asm("int3");
}
