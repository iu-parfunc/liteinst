
/*
  Benchmark description:
  Measures average cost of probe activation and deactivation via ProbeProvider 
  API.
   */

#include "fastinst.hpp"
#include "cycle.h"

#include <string>
#include <cassert>
#include <cstdlib>

using namespace std;

const uint64_t INVOCATION_COUNT = 10000;
const uint64_t FOO_FUNC_ID = 0;

int foo_count;
int foo_entry_probe_id;
int foo_exit_probe_id;

uint64_t activate_cost = 0;
uint64_t deactivate_cost = 0;

int foo(int x)
    __attribute__((noinline));
void instrumentation(ProbeArg func_id)
  __attribute__((no_instrument_function));
void callback(const ProbeMetaData* pmd)
  __attribute__((no_instrument_function));

void instrumentation(ProbeArg func_id) {

  assert(func_id == FOO_FUNC_ID);

  foo_count++;
}

void callback(const ProbeMetaData* pmd) {

  // If this callback related to foo probes we activate them
  string func_name = "_Z3fooi"; 
  if (func_name.compare(pmd->func_name) == 0) {
    if (pmd->probe_context == ProbeContext::ENTRY) {
      foo_entry_probe_id = pmd->probe_id;
      fprintf(stderr, "Foo entry probe id : %lu\n", foo_entry_probe_id);
    } else {
      foo_exit_probe_id = pmd->probe_id;
      fprintf(stderr, "Foo exit probe id : %lu\n", foo_exit_probe_id);
    }

    PROBE_PROVIDER->initialize(pmd->probe_id, FOO_FUNC_ID);
    try {
      PROBE_PROVIDER->activate(pmd->probe_id, instrumentation);
    } catch (int e) {
      fprintf(stderr, "Error while activating probe of function : %s.\n",
          pmd->func_name.c_str());
      exit(EXIT_FAILURE);
    }
  }

}

int foo(int x) {
  // Do some calculation.
  int y = 2;
  if (x != 0) {
    return x;
  }
  return x+y;
}

int main() {

  fprintf(stderr, "Benchmark probe activation and deactivation..\n");

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

  foo(0);

  for (int i=0; i<INVOCATION_COUNT; i++) {
    ticks start = getticks();
    p->deactivate(foo_entry_probe_id);
    p->deactivate(foo_exit_probe_id);
    ticks end = getticks();

    deactivate_cost += (end - start);

    foo(i);

    start = getticks();
    p->activate(foo_entry_probe_id, instrumentation);
    p->activate(foo_exit_probe_id, instrumentation);
    end = getticks();

    activate_cost += (end - start);

    foo(i);
  }

  fprintf(stderr, "Foo count : %lu\n", foo_count);
  // Check if the probes has been deactivated as expected
  assert(foo_count == INVOCATION_COUNT*2 + 3); 
  // Why +3 expected +2 for additional initial call.

  fprintf(stderr, "Number of probes deactivated : %lu\n", INVOCATION_COUNT * 2);
  fprintf(stderr, "Number of probes activated : %lu\n", INVOCATION_COUNT * 2);
  fprintf(stderr, "Average probe deactivation cost (cycles): %lu\n", 
      deactivate_cost / (INVOCATION_COUNT * 2));
  fprintf(stderr, "Average probe activation cost (cycles): %lu\n", 
      activate_cost / (INVOCATION_COUNT * 2));

  delete(p);

  exit(EXIT_SUCCESS);

}
