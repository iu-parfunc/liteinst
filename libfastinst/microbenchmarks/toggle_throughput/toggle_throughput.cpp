
/*
   Benchmark description:
   Measures toggle throughput for active/ active patching via fastinst API
   */

#include "fastinst.hpp"
#include "cycle.h"
#include "func.hpp"

#include <pthread.h>

#include <string>
#include <cassert>
#include <cstdlib>

using namespace std;

/* Control */
volatile int g_running = 0;
volatile int g_start = 0;

long num_runners = 1;
long num_iterations = 1000000;

long entry_probe_id = -1;
long exit_probe_id = -1;
long foo_count = 0;
long bar_count = 0;
long FUNC_ID = 0;

string func_mangled = "_Z4funci"; 

void *runner(void *arg) {

  // Wait until main thread gives go signal
  while (!g_start) ;

  while(g_running){
    /* the call site that we patch is within fun */

    int value = *((int*)arg);

    func(value);
  }
}

/*
   void instrumentation(ProbeArg func_id)
   __attribute__((no_instrument_function));
   void callback(const ProbeMetaData* pmd)
   __attribute__((no_instrument_function));
   */

void foo(ProbeArg func_id) {
  foo_count++;
}

void bar(ProbeArg func_id) {
  bar_count++;
}

void callback(const ProbeMetaData* pmd) {

  if (func_mangled.compare(pmd->func_name) == 0) {
    PROBE_PROVIDER->initialize(pmd->probe_id, FUNC_ID);
    if (pmd->probe_context == ProbeContext::ENTRY) {
      entry_probe_id = pmd->probe_id;
      // fprintf(stderr, "Foo entry probe id : %lu\n", foo_entry_probe_id);
    } else {
      exit_probe_id = pmd->probe_id;
      // fprintf(stderr, "Foo exit probe id : %lu\n", foo_exit_probe_id);
    }

    try {
      PROBE_PROVIDER->activate(pmd->probe_id, foo);
    } catch (int e) {
      fprintf(stderr, "Error while activating probe of function : %s.\n",
          pmd->func_name.c_str());
      exit(EXIT_FAILURE);
    }
  } else {
    assert(false); // This shouldn't happen
  }

}

int main(int argc, char* argv[]) {

  fprintf(stderr, "Benchmark probe toggle throughput..\n");

  if (argc == 1) {
    printf("NO ARGS: Running with default settings # threads : %ld " 
      " # iterations %ld..\n", num_runners, num_iterations); 
  } else {
    num_runners = atoi(argv[1]);
    num_iterations = atoi(argv[2]);
    printf("Running with # threads : %ld # iterations :%ld ..\n", 
        num_runners, num_iterations); 
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

  func(0);

  int *ids = new int[num_runners];

  g_running = 1;
  pthread_t runners[num_runners];
  int rc;
  for (int i=0; i<num_runners; i++) {
    rc = pthread_create(&runners[i], 
        NULL, 
        runner, (void *)&ids[i]);
  }

  g_start = 1;

  for (int i=0; i<100000; i++) {
    p->activate(entry_probe_id, bar);
    p->activate(exit_probe_id, bar);

    p->activate(entry_probe_id, foo);
    p->activate(exit_probe_id, foo);
  }

  g_running = 0;

  for (int i = 0; i < num_runners; i ++) {
    pthread_join(runners[i],NULL);
  }

  delete(p);
  delete(ids);

  fprintf(stderr, "Number of toggles : %lu\n", num_iterations*4);
  fprintf(stderr, "Foo count : %lu\n", foo_count);
  fprintf(stderr, "Bar count : %lu\n", bar_count);

  exit(EXIT_SUCCESS);

}
