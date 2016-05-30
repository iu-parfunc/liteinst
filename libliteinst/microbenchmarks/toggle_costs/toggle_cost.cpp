
/*
  Benchmark description:
  Measures average probe activation and deactivation costs of a probe via Finstrumentor ProbeProvider API.
   */

#include "fastinst.hpp"
#include "cycle.h"
#include "funcs.hpp"

#include <string>
#include <cassert>
#include <cstdlib>

using namespace std;

long invoke_count = 0;
long max_probe_id = 0;
long DEFAULT_FUNC_ID = 1;
long NUM_FUNCS = 10000;

ticks* deactivation_costs;
ticks* activation_costs;

void instrumentation(ProbeArg func_id)
  __attribute__((no_instrument_function));
void callback(const ProbeMetaData* pmd)
  __attribute__((no_instrument_function));

void instrumentation(ProbeArg func_id) {
  invoke_count++;
}

void callback(const ProbeMetaData* pmd) {

  // printf("probe id : %lu\n", pmd->probe_id);
  assert(pmd->probe_id == 0 || pmd->probe_id == max_probe_id + 1);
  // if(pmd->probe_id > max_probe_id) {
    max_probe_id = pmd->probe_id;
  // }

  PROBE_PROVIDER->initialize(pmd->probe_id, DEFAULT_FUNC_ID);
  try {
    PROBE_PROVIDER->activate(pmd->probe_id, instrumentation);
  } catch (int e) {
    fprintf(stderr, "Error while activating probe of function : %s.\n",
        pmd->func_name.c_str());
    exit(EXIT_FAILURE);
  }

}

int main(int argc, char* argv[]) {

  fprintf(stderr, "Benchmark probe initialization..\n");

  if (argc == 1) {
    printf("NO ARGS: Running with default NUM_FUNCS : %ld..\n", NUM_FUNCS); 
  } else {
    NUM_FUNCS = atoi(argv[1]);
    printf("Running with NUM_FUNCS : %ld..\n", NUM_FUNCS); 
  }

  ProbeProvider* p;
  try {
    p = initializeGlobalProbeProvider(ProviderType::FINSTRUMENT, callback);
  } catch (int e) {
    fprintf(stderr, "ProbeProvider already initialized. Getting the existing"
        " one..\n");
    p = getGlobalProbeProvider();
  }

  if (p == NULL) {
    fprintf(stderr, "Unable to initialize probe provider..\n");
    exit(EXIT_FAILURE);
  }

  emptyFunc0();

  deactivation_costs = new ticks[max_probe_id]();
  activation_costs   = new ticks[max_probe_id]();

  ticks start, end;  
  ticks deactivation_sum = 0;
  ticks activation_sum = 0;

  FILE* fp1 = fopen("toggle_cost_deactivation.csv", "w");
  FILE* fp2 = fopen("toggle_cost_activation.csv", "w");
 
  for (int i=0; i<max_probe_id; i++) {
    start = getticks();
    p->deactivate(i);
    end = getticks();

    deactivation_costs[i] = (end - start);

    // Filtering out outliers
    if (deactivation_costs[i] < 10000) {
      deactivation_sum += deactivation_costs[i];
      fprintf(fp1, "%d, %llu\n", i, deactivation_costs[i]);
    }
  }

  for (int i=0; i<max_probe_id; i++) {
    start = getticks();
    p->activate(i, instrumentation);
    end = getticks();

    activation_costs[i] = (end - start);
    
    // Filtering out outliers
    if (activation_costs[i] < 10000) {
      activation_sum += activation_costs[i];
      fprintf(fp2, "%d, %llu\n", i, activation_costs[i]);
    }
  }

  fprintf(stderr, "Number of functions : %ld\n", NUM_FUNCS);

  fprintf(stderr, "Probe deactivation cost estimate (cycles) : %llu\n", 
      deactivation_sum / max_probe_id);

  fprintf(stderr, "Probe activation cost estimate (cycles) : %llu\n", 
      activation_sum / max_probe_id);

  fclose(fp1);
  fclose(fp2);

  delete(p);
  delete(deactivation_costs);
  delete(activation_costs);

  exit(EXIT_SUCCESS);

}
