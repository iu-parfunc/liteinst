
#include "fastinst.hpp"

#include <assert>
#include <cstdlib>

ProbeProvider p;
int foo_count;
int foo_entry_probe_id;
int foo_exit_probe_id;

__attribute__((no_instrument_function))
void instrumenation(ProbeArg func_id) {

  assert(func_id == 0);

  foo_count++;
}

__attribute__((no_instrument_function))
void callback(ProbeMetaData* pmd) {

  if (pmd->probe_context == ProbeContext::ENTRY) {
    foo_entry_probe_id = pmd->probe_id;
  } else {
    foo_exit_probe_id = pmd->probe_id;
  }

  p->initialize(pmd->probe_id, 0);
  p->activate(pmd->probe_id, instrumentation);

}

int foo(int x) {
  // Do some calculation.
  int y = rand() % x + 3;
  return x+y;
}

int main() {

  p = new FinstrumentProbeProvider(callback);
  
  foo_count = 0;
  for (int i=0; i<100; i++) {
    foo();
  }

  assert(foo_count == 100);

  p->deactivate(foo_entry_probe_id);
  p->deactivate(foo_exit_probe_id);

  for (int i=0; i<100; i++) {
    foo();
  }

  assert(foo_count == 100);

  p->activate(foo_entry_probe_id, instrumentation);
  p->activate(foo_exit_probe_id, instrumentation);

  for (int i=0; i<100; i++) {
    foo();
  }

  assert(foo_count == 200);

  delete(p);
  
  exit(EXIT_SUCCESS);

}
