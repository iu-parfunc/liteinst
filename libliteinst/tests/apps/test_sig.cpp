
#include <stdio.h>
#include <signal.h>
#include <stdint.h>
#include <string.h>

extern "C" void dummy();

typedef uint8_t* Address;

void noop(int signum, siginfo_t* siginfo, void* context) {
  ucontext_t* ucontext = (ucontext_t*) context;
  Address interrupted_addr = (Address)(ucontext->uc_mcontext.gregs[REG_RIP]);
  printf("Interrupted address : %p\n", interrupted_addr);
  fflush(stdout);
  // assert(false);
  ucontext->uc_mcontext.gregs[REG_RIP] = (greg_t)interrupted_addr+1;
}

void registerHandler() {
  struct sigaction act;
  memset( &act, 0, sizeof act);
  act.sa_flags = SA_SIGINFO; 
  act.sa_sigaction = &noop;

  sigemptyset(& (act.sa_mask));
  sigaction(SIGILL, &act, NULL);

  printf("Registered the SIGILL handler..\n");
}

int main() {
  // dummy();

  registerHandler();

  while(true) {
    __asm(".byte 0x62");
    // __asm(".byte 0x62");
    // __asm(".byte 0xcc");
  }
}
