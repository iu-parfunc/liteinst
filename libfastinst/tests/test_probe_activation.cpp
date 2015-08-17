
#include "fastinst.hpp"

#include <cassert>
#include <cstdlib>

int foo_count;
int foo_entry_probe_id;
int foo_exit_probe_id;

__attribute__((no_instrument_function))
void instrumentation(ProbeArg func_id) {

  assert(func_id == 0);

  foo_count++;
}

__attribute__((no_instrument_function))
void callback(const ProbeMetaData* pmd) {

  if (pmd->probe_context == ProbeContext::ENTRY) {
    foo_entry_probe_id = pmd->probe_id;
  } else {
    foo_exit_probe_id = pmd->probe_id;
  }

  PROBE_PROVIDER->initialize(pmd->probe_id, 0);
  PROBE_PROVIDER->activate(pmd->probe_id, instrumentation);

}

int foo(int x) {
  // Do some calculation.
  int y = rand() % x + 3;
  return x+y;
}

int main() {

  ProbeProvider* p;
  try {
    p = ProbeProvider::initializeProbeProvider(ProviderType::FINSTRUMENT, callback);
  } catch (int e) {
    fprintf(stderr, "ProbeProvider already initialized. Getting the existing one..\n");
    p = ProbeProvider::getProbeProvider();
  }

  if (p == NULL) {
    fprintf(stderr, "Unable to intialize probe provider..\n");
    exit(-1);
  }
  
  foo_count = 0;
  for (int i=0; i<100; i++) {
    foo(i);
  }

  assert(foo_count == 100);

  p->deactivate(foo_entry_probe_id);
  p->deactivate(foo_exit_probe_id);

  for (int i=0; i<100; i++) {
    foo(i);
  }

  assert(foo_count == 100);

  p->activate(foo_entry_probe_id, instrumentation);
  p->activate(foo_exit_probe_id, instrumentation);

  for (int i=0; i<100; i++) {
    foo(i);
  }

  assert(foo_count == 200);

  delete(p);
  
  exit(EXIT_SUCCESS);

}
