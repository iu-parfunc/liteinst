
#include "doctest.h"
#include "liteinst.hpp"
#include "process.hpp"
#include "tests.hpp"

using namespace utils::process;
using namespace liteinst;

volatile int32_t entry_counter = 0;
volatile int32_t exit_counter = 0;

void entryInstrumentation() {
  printf("Entry..\n");
  entry_counter++;
  return;
}

void exitInstrumentation() {
  printf("Exit..\n");
  exit_counter++;
}

/******** Tests ********/
TEST_SUITE("Lite Probe Provider Tests");

TEST_CASE("+ Probe Registration Test") {

  ProbeProvider* p = initializeGlobalProbeProvider(ProviderType::LITEPROBES,
      nullptr);

  InstrumentationProvider i_provider("Hello", entryInstrumentation, 
      exitInstrumentation);

  p->registerInstrumentationProvider(i_provider);
  printf("Registered probe provider..\n");

  Coordinates coords;
  coords.setFunction(liteinst::Function("*"));
  coords.setProbePlacement(ProbePlacement::BOUNDARY);

  ProbeRegistration pr = p->registerProbes(coords, "Hello"); 

  printf("Registered probes..\n");

  Process process;

  CHECK(pr.getProbedFunctions().size() == process.getFunctions().size());
}

__attribute__((destructor))
void teardown() {
  assert(entry_counter == 6);
  assert(exit_counter == 6);
}
