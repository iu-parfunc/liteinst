#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <stdlib.h>
#include <pthread.h>

#include <string>

#include "liteinst.hpp"

#include "cycle.h"
#include "funcs.hpp"

using namespace liteinst;

using std::string;
using std::to_string;

int n_funcs = 0;
int n_runners = 16;

ProbeRegistration pr;

ProbeProvider* p;

void bar() {
  // printf("I am bar()\n");
  return;
}

int main(int argc, const char* argv[]) {

  n_runners = atoi(argv[1]);
  n_funcs = atoi(argv[2]);

  printf("Number of functions : %d\n", n_funcs);
  printf("Number of runners : %d\n", n_runners);

  // Setting up the probe provider and the instrumentation
  p = liteinst::ProbeProvider::initializeGlobalProbeProvider(
      ProviderType::LITEPROBES, nullptr, nullptr);
  InstrumentationProvider i_provider("Counter", bar, bar);

  p->registerInstrumentationProvider(i_provider);
  printf("[main] Registered probe provider..\n");


  // Thread fork
  pthread_t runners[n_runners]; 
  int rc;
  int *ids = new int[n_runners];

  for (int i=0; i < n_runners; i++) {
    ids[i] = i;
    int rc = pthread_create(&runners[i], NULL, synced_runner, (void*)&ids[i]);
    if (rc) {
      printf("Error creating thread : %d\n", i);
      exit(-1);
    }
  }

  while (g_registered_count < n_runners);

  ticks total = 0;
  ticks injection_costs = 0;
  ticks meta_data_costs = 0;
  ticks punning_costs = 0;

  // Instrument function entries. This should generate trampolines
  int n_failures = 0;
  __sync_bool_compare_and_swap(&g_go, g_go, 1);

  for (int i=0; i < n_funcs; i++) {
    // Specifying probe coordinates
    Coordinates coords;
    coords.setFunction(liteinst::Function("func"+ to_string(i))). 
         setProbePlacement(ProbePlacement::ENTRY);

    // Register probe meta data and inject them.
    pr = p->registerProbes(coords, "Counter"); 

    if (!pr.failures) {
      total += pr.probing_costs;
      injection_costs += pr.injection_costs;
      meta_data_costs += pr.meta_data_costs;
      punning_costs += pr.punning_costs;
    } else {
      n_failures++;
    }
  }

  printf("[Trampoline-Injection] Failures : %d\n", n_failures);
  ticks probing_cost = total / (n_funcs - n_failures);
  ticks injection_cost = injection_costs / (n_funcs - n_failures);
  ticks meta_data_cost = meta_data_costs / (n_funcs - n_failures);
  ticks punning_cost = punning_costs / (n_funcs - n_failures);

  printf("PROBING_COST : %d\n", probing_cost);
  printf("INSERTION_COST : %d\n", injection_cost);
  printf("METADATA_COST : %d\n", meta_data_cost);
  printf("PUNNING_COST : %d\n", punning_cost);

  return 0;
}
