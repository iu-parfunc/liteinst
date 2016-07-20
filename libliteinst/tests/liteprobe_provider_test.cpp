
#include "doctest.h"
#include "liteinst.hpp"
#include "process.hpp"
#include "tests.hpp"

using namespace utils::process;
using namespace liteinst;

volatile int32_t entry_counter = 0;
volatile int32_t exit_counter = 0;

static ProbeProvider* p = nullptr;

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

  if (p == nullptr) {
    p = initializeGlobalProbeProvider(ProviderType::LITEPROBES,
        nullptr);
  }

  InstrumentationProvider i_provider("i_1", entryInstrumentation, 
      exitInstrumentation);

  p->registerInstrumentationProvider(i_provider);
  printf("Registered probe provider..\n");

  Coordinates coords;
  coords.setFunction(liteinst::Function("*"));
  coords.setProbePlacement(ProbePlacement::BOUNDARY);

  ProbeRegistration pr = p->registerProbes(coords, "i_1"); 

  printf("Registered probes..\n");

  Process process;

  CHECK(pr.getProbedFunctions().size() == process.getFunctions().size());
}

TEST_CASE("+ Super Trampoline Test") {

  if (p == nullptr) {
    p = initializeGlobalProbeProvider(ProviderType::LITEPROBES,
      nullptr);
  }

  InstrumentationProvider i_provider("i_2", entryInstrumentation, 
      exitInstrumentation);

  p->registerInstrumentationProvider(i_provider);
  printf("Registered probe provider..\n");

  Coordinates coords;
  coords.setFunction(liteinst::Function("*"));
  coords.setProbePlacement(ProbePlacement::BOUNDARY);

  ProbeRegistration pr = p->registerProbes(coords, "i_2"); 

  printf("Registered probes..\n");

  Process process;

  CHECK(pr.getProbedFunctions().size() == process.getFunctions().size());
}

__attribute__((destructor))
void teardown() {
  assert(entry_counter == 2);
  assert(exit_counter == 2);
}
