
#include <assert.h>
#include <signal.h>
#include <pthread.h>
#include <iostream>
#include <cstring>

/* Test multiple threads getting synchronous interrupts.
 * Behavior
 *  Several threads in a process can generate and handle the same type of trap
 *  simultaneously.
 */

using std::cout;

void trap_handler(int signo, siginfo_t* inf, void* ptr) {
  cout << "IN_TRAP\n"; // unsafe
  while(true);
}

void* trap(void* arg) {
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
