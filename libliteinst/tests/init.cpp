
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
}

}
}
