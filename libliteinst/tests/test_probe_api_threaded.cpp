
/*
  Test description:
  Multi threaded probe activation + deactivation test.

  This tests basic workings of the ProbeProvider API with concurrent
  access.

  Function foo is instrumented with a function which increments a global
  counter each time it gets invoked. Global counter is asserted upon the
  expected counter value to be less than the total number of invocations
  due to probe deactivations. */

#include "fastinst.hpp"
#include <pthread.h>

#include <string>
#include <cassert>
#include <cstdlib>

using namespace std;

/* constants */
const int NUM_INVOCATIONS = 1000000;
const int NUM_ACTIVATIONS = 1000;
const int NUM_DEACTIVATIONS = 1000;

/* control */
volatile int g_running = true;
volatile bool g_probes_uninitialized = true;
volatile bool activator_done = false;

int foo_count = 0;
int foo_entry_probe_id;
int foo_exit_probe_id;

void* activator(void* arg)
  __attribute__((no_instrument_function));
void* deactivator(void* arg)
  __attribute__((no_instrument_function));
void instrumentation(ProbeArg func_id)
  __attribute__((no_instrument_function));
void callback(const ProbeMetaData* pmd)
  __attribute__((no_instrument_function));

void instrumentation(ProbeArg func_id) {

  assert(func_id == 0);
  
  __sync_add_and_fetch(&foo_count, 1);
}

void* activator(void* arg) {

  while(g_probes_uninitialized); /* wait until the setup is done */

  for (int i = 0; i < NUM_ACTIVATIONS; i++) {
    PROBE_PROVIDER->activate(foo_entry_probe_id, instrumentation);
    PROBE_PROVIDER->activate(foo_exit_probe_id, instrumentation);
  }

  activator_done = true;

  return NULL;
}

void* deactivator(void* arg) {

  while(g_probes_uninitialized); /* wait until the setup is done */

  for (int i = 0; i < NUM_DEACTIVATIONS; i++) {
    PROBE_PROVIDER->deactivate(foo_entry_probe_id);
    PROBE_PROVIDER->deactivate(foo_exit_probe_id);
  }

  while (!activator_done);
  g_running = false;

  return NULL;

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

    if (pmd->probe_context == ProbeContext::EXIT) {
      g_probes_uninitialized = false;
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

  fprintf(stderr, "Test concurrent probe activation and deactivation..\n");

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

  pthread_t thread1, thread2;
  int r1,r2;

  pthread_create(&thread1,
                 NULL,
                 activator,
                 (void*) &r1);

  pthread_create(&thread2,
                 NULL,
                 deactivator,
                 (void *) &r2);

  foo_count = 0;
  for (int i=0; i< NUM_INVOCATIONS; i++) {
    foo(i);
  }

  pthread_join(thread1, NULL);
  pthread_join(thread2, NULL);

  // Check the global counter. Should be less than twice the function invoaction
  // count due to probe deactivations. A single call to foo would increment 
  // the counter twice at prolog and epilog hence twice the invocation count.
  assert(foo_count < 2 * NUM_INVOCATIONS);

  delete(p);

  exit(EXIT_SUCCESS);

}
