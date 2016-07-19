
#define DOCTEST_CONFIG_IMPLEMENT 
#include "doctest.h"
#include "signals.hpp"
#include "defs.hpp"
#include "control_flow_router.hpp"
#include "tests.hpp"
#include <unistd.h>
#include <iostream>

volatile sig_atomic_t flag = 0;

namespace liteinst {
namespace liteprobes {

using namespace utils::signals;
using utils::Address;

void liteprobes_sigill_handler(int signum, siginfo_t* siginfo, void* context) {
  printf("In SIGILL..\n");
  // Get the interrupted instruction
  ucontext_t *ucontext = (ucontext_t*)context;
  Address interrupted_addr = (Address)(ucontext->uc_mcontext.gregs[REG_RIP]);

  // Reroute to the new address within a springboard
  Address reroute_addr = liteinst::liteprobes::
    ControlFlowRouter::getRerouteAddress(interrupted_addr);

  printf("Rerouting to : %p\n", reroute_addr);
  ucontext->uc_mcontext.gregs[REG_RIP] = (greg_t)reroute_addr;
}

void init() {

  // Register the SIGILL handler
  struct sigaction act;
  act.sa_sigaction = liteprobes_sigill_handler;

  HandlerRegistration reg;
  reg.signum = SIGILL;
  reg.act = act;

  SignalHandlerRegistry::registerSignalHandler(reg);

  printf(".....\n");


  doctest::Context context(0, nullptr);

  int res = context.run();
  
  if(context.shouldExit()) { 
    return;  
  }

  // printf("HEllow wordl!\n");

  /*
  struct sigaction act;
  memset( &act, 0, sizeof act);
  act.sa_sigaction= & trap_handler;
  act.sa_flags = SA_SIGINFO;
  sigemptyset(& (act.sa_mask));

  sigaction(SIGINT, &act, NULL);

  int counter = 1;
  while(counter--) {
    sleep(1);
  }
  */
}

}
}
