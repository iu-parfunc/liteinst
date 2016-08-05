
#include <pthread.h>

#include "liteinst.hpp"
#include "process.hpp"

using namespace liteinst;
using namespace utils::process;

#define NUM_THREADS 16

volatile bool done = false;
volatile int64_t foo_count = 0;
volatile int64_t real_count = 0;

ProbeRegistration pr;

// Instrumentation function
void foo() {
  ProbeInfo pi;
  LITEINST_SET_PROBE_INFO(pi);

  foo_count++; // Should ideally be CAS. 
}

// Instrumented function
__attribute__ ((noinline))
void func() {
}

void* runner(void* arg) { 
  while (!done) {
    func();
    real_count++;
  }
}

void* toggler(void* arg) {
  ProbeProvider* p = liteinst::ProbeProvider::getGlobalProbeProvider();
  while (foo_count < 1000000) {
    p->activate(pr);
    p->deactivate(pr);
  }

  done = true;
}

int main() {
  // Setting up the probe provider and the instrumentation
  ProbeProvider* p = liteinst::ProbeProvider::initializeGlobalProbeProvider(
      ProviderType::LITEPROBES, nullptr, nullptr);
  InstrumentationProvider i_provider("i_1", foo, 
      nullptr);

  p->registerInstrumentationProvider(i_provider);
  printf("Registered probe provider..\n");

  // Specifying probe coordinates
  Coordinates coords;
  coords.setFunction(liteinst::Function("_Z4funcv")).
    setProbePlacement(ProbePlacement::ENTRY);

  // Register probe meta data and inject them.
  pr = p->registerProbes(coords, "i_1"); 

  printf("Registered probes..\n");

  Process process;

  assert(pr.getProbedFunctions().size() == 1);

  pthread_t threads[NUM_THREADS]; 

  for (int i=0; i < NUM_THREADS; i++) {
    int rc = pthread_create(&threads[i], NULL, runner, (void*) i);
    if (rc) {
      printf("Error creating thread : %d\n", i);
      exit(-1);
    }
  }

  pthread_t toggle;
  int rc = pthread_create(&toggle, NULL, toggler, NULL);
  if (rc) {
    printf("Error creating toggler thread\n");
    exit(-1);
  }

  void* status;
  for (int i=0; i < NUM_THREADS; i++) {
    int rc = pthread_join(threads[i], &status);
  }

  pthread_join(toggle, &status);

  assert(real_count > foo_count);
  
  printf("Sample count : %ld\n", foo_count);
  printf("Invocation count : %ld\n", real_count);

}
