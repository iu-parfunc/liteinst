
#include "liteinst.hpp"
#include "process.hpp"

using namespace liteinst;
using namespace utils::process;

int64_t counter = 0;

struct ProfileData {
  int64_t count;
};

ProfileData* stats;

__attribute__((destructor))
void tear_down() {
  printf("Counter value : %lu\n", counter);
}

void entryInstrumentation() {
  printf("Entry..\n");
  counter++;
  stats[0].count++;
  return;
}

void exitInstrumentation() {
  printf("Exit..\n");
  counter++;
  stats[0].count++;
}

void initCallback() {
  printf("At init call back..\n");
  ProbeProvider* p = liteinst::ProbeProvider::getGlobalProbeProvider();

  InstrumentationProvider i_provider("Sampling", entryInstrumentation, 
      exitInstrumentation);

  p->registerInstrumentationProvider(i_provider);
  printf("Registered probe provider..\n");

  Coordinates coords;
  coords.setFunction(liteinst::Function("*"));
  coords.setProbePlacement(ProbePlacement::BOUNDARY);

  ProbeRegistration pr = p->registerProbes(coords, "Sampling"); 

  printf("Registered probes..\n");

  // Process process;

  // unsigned int num_funcs = process.getFunctions().size();
  // assert(pr.getProbedFunctions().size() == num_funcs);

  stats = new ProfileData[1];
  stats[0].count = 0;
}

__attribute__((constructor))
void initProfiler() {
  ProbeProvider* p = liteinst::ProbeProvider::initializeGlobalProbeProvider(
      ProviderType::LITEPROBES, nullptr, initCallback);
}
