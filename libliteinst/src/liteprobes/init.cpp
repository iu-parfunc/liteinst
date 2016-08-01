
#include "liteinst.hpp"
#include "process.hpp"
#include "control_flow_router.hpp"
#include "signals.hpp"

namespace liteinst {
namespace liteprobes {

using namespace utils::signals;

void premain() {

  // Register the SIGILL handler
  struct sigaction act;
  act.sa_sigaction = liteprobes_sigill_handler;

  HandlerRegistration reg;
  reg.signum = SIGILL;
  reg.act = act;

  SignalHandlerRegistry::registerSignalHandler(reg);

  printf("Installed signal handler..\n");

  ProbeProvider *p = ProbeProvider::getGlobalProbeProvider();
  if (p != nullptr && p->init_callback != nullptr) {
    p->init_callback();
  }

  /*
  ProbeProvider *p = ProbeProvider::getGlobalProbeProvider(ProviderType::LITEPROBES,
      nullptr);

  InstrumentationProvider i_provider("i_2", entryInstrumentation1, 
      exitInstrumentation1);

  p->registerInstrumentationProvider(i_provider);
  printf("Registered probe provider..\n");

  Coordinates coords;
  coords.setFunction(Function("*"));
  coords.setProbePlacement(ProbePlacement::BOUNDARY);

  ProbeRegistration pr = p->registerProbes(coords, "i_2"); 

  printf("Registered probes..\n");
  */

  // CHECK(pr.getProbedFunctions().size() == process.getFunctions().size());
}

}
}
