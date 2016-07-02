
#include "doctest.h"
#include "signals.hpp"
#include <unistd.h>
#include <pthread.h>

using namespace utils::signals;

volatile sig_atomic_t signalled = 0;

/******* Helper functions *******/
void trap_handler(int signum, siginfo_t* siginfo, void* context) {
  signalled = 1;
}

/******** Tests *********/
TEST_SUITE("Signal Handling Tests");

TEST_CASE("+ Synchronous signal handler registration test") {

  // Register handler
  struct sigaction act;
  act.sa_sigaction = trap_handler;

  HandlerRegistration reg;
  reg.signum = SIGTRAP;
  reg.act = act;

  SignalHandlerRegistry::registerSignalHandler(reg);

  // Generate a signal
  // __asm__("int3");
  raise(SIGTRAP);

  CHECK(signalled == 1);

  SignalHandlerRegistry::unregisterSignalHandler(reg);

  struct sigaction newact;
  sigaction(SIGTRAP, NULL, &newact);

  CHECK(newact.sa_handler == SIG_DFL);

}

// + Asynchronous signal handler registration test
// + Multiple synchronous handler registration test
// + Multiple asynchroous handler registration test
// + Multiple synchronous and asynchrounous handler registration test
// + Multiple synchronous handler registration with priority test
// + Multiple asynchronous handler registration with priority test
// + Multiple synchronous and asynchronous handler registration with priority test
// + sigaction registration priority handling test
// + sigaction registration override test
