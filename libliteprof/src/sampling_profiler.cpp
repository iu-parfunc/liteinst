
#include "liteinst.hpp"
#include "process.hpp"

using namespace liteinst;
using namespace utils::process;

struct ProfileData {
  int64_t count;
};

ProfileData* stats;

void entryInstrumentation() {
  stats[0].count++;
  return;
}

void exitInstrumentation() {
  stats[0].count++;
}

void initCallback() {
  ProbeProvider* p = liteinst::ProbeProvider::getGlobalProbeProvider(
      ProviderType::LITEPROBES, nullptr, initCallback);

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
  ProbeProvider* p = liteinst::ProbeProvider::getGlobalProbeProvider(
      ProviderType::LITEPROBES, nullptr, initCallback);
}
