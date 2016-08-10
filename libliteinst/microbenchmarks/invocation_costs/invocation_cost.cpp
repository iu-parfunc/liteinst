
/*
  Benchmark description:
  Measures probe invocation costs 
 */

#include <pthread.h>
#include <limits.h>

#include "liteinst.hpp"
#include "process.hpp"

#include "cycle.h"

using namespace liteinst;
using namespace utils::process;

#define NS_PER_S 1000000000
#define PAD 8
// This must be odd:
// This fixes the worst of the imbalance problem, but a problem still remains:
// #define BURST_SIZE 997
// This is much better for balance, but worse for maximum toggle rate:
#define BURST_SIZE 1

#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))
#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))

unsigned long n_invocations=100000000000;

ProbeRegistration pr;

ProbeProvider* p;

// Instrumented function
__attribute__ ((noinline))
void func() {
}

// Instrumentation function
void foo() {
}

double run_experiment() {
  struct timespec t1, t2;
  int clock_mode = CLOCK_MONOTONIC;

  // Then GO!
  long start = getticks();

  t2 = t1;
  for(int i=0; i<n_invocations; i++) {
    foo();
  }

  long end = getticks();

  printf("\nFinally, here is some human-readable output, not for HSBencher:\n");
  setlocale(LC_NUMERIC, "");
  printf("Number of invocations : %lu\n", n_invocations);
  printf("Cost per invocation (cycles) : %lu\n", (end - start) / n_invocations); 
}

int main(int argc, char* argv[]) {

  fprintf(stderr, "Benchmark probe invocation costs..\n");

  if (argc < 2) {
    printf("NOT ENOUGH ARGS, expects 1: invocations \n");
    //    "\nRunning with default settings # threads : %ld # iterations %ld..\n", num_runners, target_rate
    return 1;
  } else {
    n_invocations  = atoi(argv[1]);
  }

  // Setting up the probe provider and the instrumentation
  p = liteinst::ProbeProvider::initializeGlobalProbeProvider(
      ProviderType::LITEPROBES, nullptr, nullptr);
  InstrumentationProvider i_provider("Counter", foo, 
      nullptr);

  p->registerInstrumentationProvider(i_provider);
  printf("[main] Registered probe provider..\n");

  // Specifying probe coordinates
  Coordinates coords;
  coords.setFunction(liteinst::Function("_Z4funcv")).
    setProbePlacement(ProbePlacement::ENTRY);

  // Register probe meta data and inject them.
  pr = p->registerProbes(coords, "Counter"); 

  Process process;

  assert(pr.getProbedFunctions().size() == 1);

  printf("[main] Registered probes..\n");

  // Then we can RUN
  // ---------------

  run_experiment();
  run_experiment();

  exit(EXIT_SUCCESS);

}
