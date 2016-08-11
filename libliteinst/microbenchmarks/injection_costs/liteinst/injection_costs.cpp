
/*
  Benchmark description:
  Measures cost of injecting probes to a running application 
 */

#include <pthread.h>
#include <unistd.h>
#include <sched.h>
#include <limits.h>

#include "liteinst.hpp"
#include "process.hpp"
#include "funcs.hpp"

#include "cycle.h"

using namespace liteinst;
using namespace utils::process;

using std::to_string;

int NUM_CORES;

unsigned long n_funcs = 120;

ProbeRegistration pr;

ProbeProvider* p;

// Thread utilities

int stick_this_thread_to_core(int core_id) {
  if (core_id < 0 || core_id >= NUM_CORES)
    return EINVAL;

  printf("Sticking thread to core : %d\n", core_id);

  cpu_set_t cpuset;
  CPU_ZERO(&cpuset);
  CPU_SET(core_id, &cpuset);

  pthread_t current_thread = pthread_self();    
  return pthread_setaffinity_np(current_thread, sizeof(cpu_set_t), &cpuset);
}

// Instrumentation function
void foo() {
}

int main(int argc, char* argv[]) {
  fprintf(stderr, "Benchmark probe injection costs..\n");

  if (argc < 2) {
    printf("NOT ENOUGH ARGS, expects 1: # funcs \n");
    //    "\nRunning with default settings # threads : %ld # iterations %ld..\n", num_runners, target_rate
    return 1;
  } else {
    n_funcs = atoi(argv[1]);
  }

  NUM_CORES = sysconf(_SC_NPROCESSORS_ONLN);

  printf("Running benchmark with %d functions..\n", n_funcs);

  // Setting up the probe provider and the instrumentation
  p = liteinst::ProbeProvider::initializeGlobalProbeProvider(
      ProviderType::LITEPROBES, nullptr, nullptr);
  InstrumentationProvider i_provider("Counter", foo, foo);

  p->registerInstrumentationProvider(i_provider);
  printf("[main] Registered probe provider..\n");

  stick_this_thread_to_core(0);

  Coordinates coords;
  coords.setFunction(liteinst::Function("func.*")).
      setProbePlacement(ProbePlacement::ENTRY);

  // Instrument function entries. This should generate trampolines
  // Register probe meta data and inject them.
  pr = p->registerProbes(coords, "Counter"); 

  printf("[Trampoline-Injection] Failures : %d\n", pr.num_failed_pgs);
  ticks tramp_cost = pr.injection_cost / pr.num_probed_pgs;

  printf("TRAMPOLINE_INJECTION_COST : %ld\n", tramp_cost);
  // printf("SUPER_TRAMPOLINE_INJECTION_COST : %ld\n", super_tramp_cost);

  exit(EXIT_SUCCESS);

}
