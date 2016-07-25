
#include "liteinst.hpp"
#include "process.hpp"
#include "control_flow_router.hpp"
#include "signals.hpp"

static liteinst::ProbeProvider* p = nullptr;

volatile sig_atomic_t entry_counter = 0;
volatile sig_atomic_t exit_counter = 0;

void entryInstrumentation1() {
  // printf("Entry..\n");
  entry_counter++;
  return;
}

void exitInstrumentation1() {
  // printf("Exit..\n");
  exit_counter++;
}

__attribute__((destructor))
void teardown() {
  // assert(entry_counter == 269932720);
  // assert(exit_counter == 269932720);

  printf("ENTRY : %lu EXIT : %lu\n", entry_counter, exit_counter);
  printf("DONE..\n");
}

namespace liteinst {
namespace liteprobes {

using namespace utils::signals;

void init() {

  // Register the SIGILL handler
  struct sigaction act;
  act.sa_sigaction = liteprobes_sigill_handler;

  HandlerRegistration reg;
  reg.signum = SIGILL;
  reg.act = act;

  SignalHandlerRegistry::registerSignalHandler(reg);

  printf("Installed signal handler..\n");

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

  // CHECK(pr.getProbedFunctions().size() == process.getFunctions().size());
}

}
}
