
/*
  Benchmark description:
  Measures average probe activation and deactivation costs of a probe via Finstrumentor ProbeProvider API.
   */

#include "liteinst.hpp"
#include "process.hpp"
#include "cycle.h"
#include "funcs.hpp"

#include <string>
#include <cassert>
#include <cstdlib>

using namespace std;
using namespace utils::process;
using namespace liteinst;

#define OUTLIER 1000000

long invoke_count = 0;
long invoke_exit_count = 0;
long max_probe_id = 0;
long DEFAULT_FUNC_ID = 1;
long NUM_FUNCS = 10000;

ticks* deactivation_costs;
ticks* activation_costs;

/*
void instrumentation(ProbeArg func_id)
  __attribute__((no_instrument_function));
void callback(const ProbeMetaData* pmd)
  __attribute__((no_instrument_function));
*/ 

/* void instrumentation(ProbeArg func_id) { */
/*   invoke_count++; */
/* } */

void instrumentation() {
  invoke_count++;
}

void instrumentation_exit() {
  invoke_exit_count++;
}


/* void callback(const ProbeMetaData* pmd) { */

/*   // printf("probe id : %lu\n", pmd->probe_id); */
/*   assert(pmd->probe_id == 0 || pmd->probe_id == max_probe_id + 1); */
/*   // if(pmd->probe_id > max_probe_id) { */
/*     max_probe_id = pmd->probe_id; */
/*   // } */

/*   PROBE_PROVIDER->initialize(pmd->probe_id, DEFAULT_FUNC_ID); */
/*   try { */
/*     PROBE_PROVIDER->activate(pmd->probe_id, instrumentation); */
/*   } catch (int e) { */
/*     fprintf(stderr, "Error while activating probe of function : %s.\n", */
/*         pmd->func_name.c_str()); */
/*     exit(EXIT_FAILURE); */
/*   } */

/* } */

void callback(const ProbeInfo* pi) {

  /* // printf("probe id : %lu\n", pmd->probe_id); */
  /* assert(pmd->probe_id == 0 || pmd->probe_id == max_probe_id + 1); */
  /* // if(pmd->probe_id > max_probe_id) { */
  /*   max_probe_id = pmd->probe_id; */
  /* // } */

  /* PROBE_PROVIDER->initialize(pmd->probe_id, DEFAULT_FUNC_ID); */
  /* try { */
  /*   PROBE_PROVIDER->activate(pmd->probe_id, instrumentation); */
  /* } catch (int e) { */
  /*   fprintf(stderr, "Error while activating probe of function : %s.\n", */
  /*       pmd->func_name.c_str()); */
  /*   exit(EXIT_FAILURE); */
  /* } */

}


int main(int argc, char* argv[]) {


  /*  fprintf(stderr, "Benchmark probe initialization..\n"); */

  if (argc == 1) {
    printf("NO ARGS: Running with default NUM_FUNCS : %ld..\n", NUM_FUNCS); 
  } else {
    NUM_FUNCS = atoi(argv[1]);
    printf("Running with NUM_FUNCS : %ld..\n", NUM_FUNCS); 
  }

  ProbeProvider* p;

  p = liteinst::ProbeProvider::initializeGlobalProbeProvider(
      ProviderType::LITEPROBES,nullptr,nullptr); 

  InstrumentationProvider i_provider("instr", instrumentation, instrumentation_exit);

  p->registerInstrumentationProvider(i_provider);
  printf("Registered probe provider..\n");
  
  Coordinates coords;
  coords.setFunction(liteinst::Function("_Z10emptyFunc0v"));
  coords.setProbePlacement(ProbePlacement::BOUNDARY);

  ProbeRegistration pr = p->registerProbes(coords, "instr"); 
  
  Process process; 

  printf("getProbedFunctions: %d\n", pr.getProbedFunctions().size()); 
  max_probe_id = pr.getProbedFunctions().size();
  
  assert(pr.getProbedFunctions().size() == NUM_FUNCS); // process.getFunctions().size());

/*
  } catch (int e) {
    fprintf(stderr, "ProbeProvider already initialized. Getting the existing"
        " one..\n");
    p = getGlobalProbeProvider();
  }
*/ 

  if (p == NULL) {
    fprintf(stderr, "Unable to initialize probe provider..\n");
    exit(EXIT_FAILURE);
  }

  emptyFunc0();

  deactivation_costs = new ticks[2*max_probe_id]();
  activation_costs   = new ticks[2*max_probe_id]();

  ticks start, end;  
  ticks deactivation_sum = 0;
  ticks activation_sum = 0;

  FILE* fp1 = fopen("toggle_cost_deactivation.csv", "w");
  FILE* fp2 = fopen("toggle_cost_activation.csv", "w");

  int i = 0; 
  
  for (const auto& it : pr.pg_by_function) { 

    for (ProbeGroupInfo pgi : it.second) {
      start = getticks();
      p->deactivate(pgi); 
      end = getticks();

      deactivation_costs[i] = (end - start);

      // Filtering out outliers
      if (deactivation_costs[i] < OUTLIER) {
        deactivation_sum += deactivation_costs[i];
        fprintf(fp1, "%d, %llu\n", i, deactivation_costs[i]);
      }
      i++;
    }
  }
 
  i = 0; 
  for (const auto& it : pr.pg_by_function) {
    for (ProbeGroupInfo pgi : it.second) { 
      start = getticks();
      p->activate(pgi);
      end = getticks();

      activation_costs[i] = (end - start);
    
      // Filtering out outliers
      if (activation_costs[i] < OUTLIER) {
        activation_sum += activation_costs[i];
        fprintf(fp2, "%d, %llu\n", i, activation_costs[i]);
      }
      i++;
    }
  }

  fprintf(stderr, "Number of functions : %ld\n", NUM_FUNCS);

// multiply by 2 because of activating/deactivating two probes per fun
  fprintf(stderr, "Probe deactivation cost estimate (cycles) : %llu\n", 
	  deactivation_sum / (2 * max_probe_id));

  fprintf(stderr, "Probe activation cost estimate (cycles) : %llu\n", 
	  activation_sum / (2 * max_probe_id));
 
  fclose(fp1);
  fclose(fp2);

  delete(p);
  delete(deactivation_costs);
  delete(activation_costs);

  exit(EXIT_SUCCESS);

}
