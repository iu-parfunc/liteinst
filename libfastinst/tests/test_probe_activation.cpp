
/*
  Test description:
  Single threaded probe activation + deactivation test.

  This tests basic workings of the ProbeProvider API without concurrent
  access.

  Function foo is instrumented with a function which increments a global
  counter each time it gets invoked. Global counter is asserted upon the
  expected counter value after a fixed number of invocations with a probe 
  deactivation and a subsequent activation in between.
   */

#include "fastinst.hpp"

#include <string>
#include <cassert>
#include <cstdlib>

using namespace std;

int foo_count;
int foo_entry_probe_id;
int foo_exit_probe_id;

void instrumentation(ProbeArg func_id)
  __attribute__((no_instrument_function));
void callback(const ProbeMetaData* pmd)
  __attribute__((no_instrument_function));

void instrumentation(ProbeArg func_id) {

  assert(func_id == 0);

  foo_count++;
}

void callback(const ProbeMetaData* pmd) {

  // If this callback related to foo probes we activate them
  string foo_mangled = "_Z3fooi"; 
  if (foo_mangled.compare(pmd->func_name) == 0) {
    int FOO_FUNC_ID = 0;
    if (pmd->probe_context == ProbeContext::ENTRY) {
      foo_entry_probe_id = pmd->probe_id;
    } else {
      foo_exit_probe_id = pmd->probe_id;
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
  int y = 0;
  if (x != 0) {
    y = rand() % x + 3;
  }
  return x+y;
}

int main() {

  ProbeProvider* p;
  try {
    p = initializeGlobalProbeProvider(ProviderType::FINSTRUMENT, callback);
  } catch (int e) {
    fprintf(stderr, "ProbeProvider already initialized. Getting the existing one..\n");
    p = getGlobalProbeProvider();
  }

  if (p == NULL) {
    fprintf(stderr, "Unable to initialize probe provider..\n");
    exit(EXIT_FAILURE);
  }

  foo_count = 0;
  for (int i=0; i<100; i++) {
    foo(i);
  }

  // Check the global counter. Should be twice the function invoaction
  // count since both prolog and epilog instrumentation would incremnt
  // the global counter.
  assert(foo_count == 200);

  p->deactivate(foo_entry_probe_id);
  p->deactivate(foo_exit_probe_id);

  for (int i=0; i<100; i++) {
    foo(i);
  }

  // If the instrumentation has been properly disabled the counter should
  // remain the same.
  assert(foo_count == 200);

  p->activate(foo_entry_probe_id, instrumentation);
  p->activate(foo_exit_probe_id, instrumentation);

  for (int i=0; i<100; i++) {
    foo(i);
  }

  // If the instrumenation has been properly reenabled the counter should have
  // been incremented now.
  assert(foo_count == 400);

  delete(p);

  fprintf(stderr, "Test passed..\n");

  exit(EXIT_SUCCESS);

}
