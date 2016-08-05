
#include "liteinst.hpp"
#include "process.hpp"

using namespace liteinst;
using namespace utils::process;

int64_t foo_count = 0;

void foo() {
  foo_count++;
}

__attribute__ ((noinline))
void func() {
}

int main() {
  ProbeProvider* p = liteinst::ProbeProvider::initializeGlobalProbeProvider(
      ProviderType::LITEPROBES, nullptr, nullptr);
  InstrumentationProvider i_provider("i_1", foo, 
      nullptr);

  p->registerInstrumentationProvider(i_provider);
  printf("Registered probe provider..\n");

  Coordinates coords;
  coords.setFunction(liteinst::Function("_Z4funcv"));
  coords.setProbePlacement(ProbePlacement::ENTRY);

  ProbeRegistration pr = p->registerProbes(coords, "i_1"); 

  printf("Registered probes..\n");

  Process process;

  assert(pr.getProbedFunctions().size() == 1);

  for (int i=0; i < 100000; i++) {
    func();
  }

  printf("Invocation count : %ld\n", foo_count);

}
