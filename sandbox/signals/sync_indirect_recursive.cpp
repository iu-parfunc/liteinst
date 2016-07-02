
#include <signal.h>
#include <cstring>
#include <cstdint>
#include <iostream>

/* Test for invoking synchronous signals recursively. Control flow is
 * synchrounous signal 1 handler -> synchronous signal 2 handler -> synchronous
 *  signal 1 handler
 *
 * Behavior is
 *   1. If SA_NODEFER is not specified signal is blocked and the program faults
 *      with an unhandled trap.
 *   2. If SA_NODEFER is specified the trap handler is entered recursively.
 */


using std::cout;

typedef uint8_t* Address;

Address handler_addr = NULL;
Address main_addr = NULL;

void int_handler(int signo, siginfo_t* inf, void* ptr) {
  cout << "IN_INT\n"; // unsafe

  handler_addr = (Address) &&handler_trap;
handler_trap:
  asm("int3");
  cout << "Synchronous interrupt lost!!\n"; // unsafe
}

void trap_handler(int signo, siginfo_t* inf, void* ptr) {
  ucontext_t *ucontext = (ucontext_t*)ptr;
  Address ip = (Address)(ucontext->uc_mcontext.gregs[REG_RIP]-1);
  
  cout << "IN_TRAP\n"; // unsafe

  if (ip == handler_addr) {
    cout << "Recursive handler invocation for SIGTRAP\n"; // unsafe
    exit(0);
  } else if (ip == main_addr) {
    cout << "Raising SIGINT\n";
    raise(SIGINT);
  } 
  while(true);
}

int main() {

  struct sigaction g_newact;
  struct sigaction g_oldact;

  memset( &g_newact, 0, sizeof g_newact);
  g_newact.sa_sigaction = & trap_handler;
  g_newact.sa_flags = SA_SIGINFO;
  sigemptyset(& (g_newact.sa_mask));

  sigaction(SIGTRAP, &g_newact, &g_oldact);

  memset( &g_newact, 0, sizeof g_newact);
  g_newact.sa_sigaction = & int_handler;
  g_newact.sa_flags = SA_SIGINFO;
  sigemptyset(& (g_newact.sa_mask));

  sigaction(SIGINT, &g_newact, &g_oldact);

  main_addr = (Address) &&main_trap;

main_trap:
  asm("int3");
}
