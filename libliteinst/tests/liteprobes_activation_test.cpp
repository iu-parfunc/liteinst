
#include "doctest.h"
#include "liteinst.hpp"
#include "process.hpp"
#include "tests.hpp"

using namespace utils::process;
using namespace liteinst;

volatile sig_atomic_t tear_down = 0;
volatile int32_t entry_counter = 0;
volatile int32_t exit_counter = 0;

void tear_down_handler(int signum) {
  if (signum == SIGUSR1) {
    tear_down = 1;
  }
}

__attribute__((constructor))
void register_tear_down_handler() {
  signal(SIGUSR1, tear_down_handler);
}

void entryInstrumentation() {
  printf("Entry..\n");
  entry_counter++;
  return;
}

void exitInstrumentation() {
  printf("Exit..\n");
  exit_counter++;
}

/******* Tests ********/
TEST_SUITE("Lite Probe Provider Tests");

TEST_CASE("+ Probe Activation Test") {

  ProbeProvider* p = liteinst::ProbeProvider::getGlobalProbeProvider(
    ProviderType::LITEPROBES, nullptr);

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

  for (const auto& it : pr.pg_by_function) {
    printf("Deactivating function : %s\n", it.first.c_str());

    for (ProbeGroupInfo pgi : it.second) {
      p->deactivate(pgi);
    }
  }

  for (const auto& it : pr.pg_by_function) {
    printf("Activating function : %s\n", it.first.c_str());

    for (ProbeGroupInfo pgi : it.second) {
      p->activate(pgi);
    }
  }
}

__attribute__((destructor))
TEST_CASE ("+ Activation Teardown") {
  if (tear_down) {
    printf("Entry counter : %d\n", entry_counter);
    printf("Exit counter : %d\n", exit_counter);
  }
  // assert(entry_counter == 1);
  // assert(exit_counter == 1);
}
