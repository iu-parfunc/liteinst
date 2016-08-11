
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

unsigned long n_funcs = 120;

int NUM_CORES;

volatile bool g_run = false;
volatile int g_stride = 0;
volatile int g_ready = 0;
volatile int g_thread_registration_count = 0;

int n_strides = 0;

ProbeRegistration pr;

ProbeProvider* p;

// Thread utilities

int stick_this_thread_to_core(int core_id) {
  if (core_id < 0 || core_id >= NUM_CORES)
    return EINVAL;

  cpu_set_t cpuset;
  CPU_ZERO(&cpuset);
  CPU_SET(core_id, &cpuset);

  pthread_t current_thread = pthread_self();    
  return pthread_setaffinity_np(current_thread, sizeof(cpu_set_t), &cpuset);
}

// Instrumentation function
void foo() {
}

void* runner(void* arg) {
  int tid = *(int*) arg;

  stick_this_thread_to_core(tid);

  __sync_add_and_fetch(&g_thread_registration_count, 1);

loop:
  while (!g_run);

  fn_ptr fn = g_funcs[g_stride * NUM_CORES + tid];
  __sync_add_and_fetch(&g_ready, 1);

  while (g_run) {
    fn();
  }

  goto loop;
}

int main(int argc, char* argv[]) {
  fprintf(stderr, "Benchmark probe injection costs with contention\n");

  NUM_CORES = sysconf(_SC_NPROCESSORS_ONLN);
  printf("Number of cores available : %d\n", NUM_CORES);
  printf("Number of cores to be used : %d\n", NUM_CORES-1);

  NUM_CORES = NUM_CORES-2;

  if (argc < 2) {
    printf("NOT ENOUGH ARGS, expects 1: # funcs \n");
    //    "\nRunning with default settings # threads : %ld # iterations %ld..\n", num_runners, target_rate
    return 1;
  } else {
    n_funcs = atoi(argv[1]);
    n_strides = n_funcs / NUM_CORES; 
    n_funcs = n_strides * NUM_CORES;
  }

   printf("Running benchmark with %d functions..\n", n_funcs);
   printf("Running benchmark with %d runners..\n", NUM_CORES);


  // Initialize function ptrs
  init();

  // Setting up the probe provider and the instrumentation
  p = liteinst::ProbeProvider::initializeGlobalProbeProvider(
      ProviderType::LITEPROBES, nullptr, nullptr);
  InstrumentationProvider i_provider("Counter", foo, foo);

  p->registerInstrumentationProvider(i_provider);
  printf("[main] Registered probe provider..\n");

  // Thread fork
  pthread_t runners[NUM_CORES]; 
  int rc;
  int *ids = new int[NUM_CORES];

  g_run = 0;
  g_stride = 0;

  for (int i=0; i < NUM_CORES; i++) {
    ids[i] = i;
    int rc = pthread_create(&runners[i], NULL, runner, (void*)&ids[i]);
    if (rc) {
      printf("Error creating thread : %d\n", i);
      exit(-1);
    }
  }

  stick_this_thread_to_core(NUM_CORES);

  // Wait until all the newly spawned threads report their existance
  while (g_thread_registration_count < NUM_CORES);

  ticks total = 0;

  // Instrument function entries. This should generate trampolines
  int n_failures = 0;
  while (g_stride < n_strides) {
    g_run = 1;

    // while (g_ready < NUM_CORES);

    for (int i=0; i < NUM_CORES; i++) {
      // Specifying probe coordinates
      Coordinates coords;
      coords.setFunction(liteinst::Function("func"+ 
            to_string(g_stride * NUM_CORES + i))).setProbePlacement(
            ProbePlacement::ENTRY);

      // Register probe meta data and inject them.
      pr = p->registerProbes(coords, "Counter"); 

      if (!pr.failures) {
        total += pr.injection_cost;
      } else {
        n_failures++;
      }
    }
    g_ready = 0;
    g_run = 0;

    // printf("Done stride : %d\n", g_stride);
    __sync_add_and_fetch(&g_stride, 1);
  }

  printf("[Trampoline-Injection] Failures : %d\n", n_failures);
  ticks tramp_cost = total / (n_funcs - n_failures);

  // reset
  total = 0;
  g_stride = 0;
  g_ready = 0;

  // Instrument function exits. This should generate super trampolines given
  // that function is less than 10 bytes.
  /*
  while (g_stride < n_strides) {
    printf("[SuperTrampoline-Injection] Trying stride : %d\n", g_stride);
    g_run = 1;

    for (int i=0; i < NUM_CORES; i++) {
      // Specifying probe coordinates
      Coordinates coords;
      coords.setFunction(liteinst::Function("func"+ 
            to_string(g_stride * NUM_CORES + i))).setProbePlacement(
            ProbePlacement::EXIT);

      ticks start = getticks();
      // Register probe meta data and inject them.
      pr = p->registerProbes(coords, "Counter"); 
      ticks end = getstart();

      if (!pr.failures) {
        total += (end - start);
      } else {
        n_failures++;
      }
    }
    g_ready = 0;
    g_run = 0;

    g_stride++;
  }

  printf("[SuperTrampoline-Injection] Failures : %d\n", n_failures);
  ticks super_tramp_cost = total / (n_funcs - n_failures);
  */

  // Process process;

  printf("TRAMPOLINE_INJECTION_COST : %ld\n", tramp_cost);
  // printf("SUPER_TRAMPOLINE_INJECTION_COST : %ld\n", super_tramp_cost);

  exit(EXIT_SUCCESS);

}
