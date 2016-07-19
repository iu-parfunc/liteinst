
#include "control_flow_router.hpp"
#include "signals.hpp"

// namespace liteinst {
// namespace liteprobes {

using namespace utils::signals;

void init() {

  // Register the SIGILL handler
  struct sigaction act;
  act.sa_sigaction = liteprobes_sigill_handler;

  HandlerRegistration reg;
  reg.signum = SIGILL;
  reg.act = act;

  SignalHandlerRegistry::registerSignalHandler(reg);

  printf("Hello.. World..\n");
}

// }
// }
