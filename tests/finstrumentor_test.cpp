
#include "../api/probe_API.hpp"
#include "assert.h"

// Thread local statistics data for current thread
__thread TLStatistics* current_thread_stats;

long call_count = 0;

void foo() {
  // Do some computation
  int x;
  x = (x++) << 2;
}

__attribute__((no_instrument_function))
TLStatistics* emptyProlog(uint16_t id) {

  static __thread bool allocated;
  if (!allocated) {
    allocated = true;
    current_thread_stats = new TLStatistics;
    current_thread_stats->thread_local_overhead = 0;
    current_thread_stats->thread_local_count = 0;
  }

  return current_thread_stats; 
} 

__attribute__((no_instrument_function))
TLStatistics* epilog(uint16_t id) {
  call_count++;
}

int main() {

  Instrumentor* instrumentor = Instrumentor::getInstance(FINSTRUMENT, emptyProlog, epilog); 

  assert(call_count == 0);

  for (int i=0; i<100; i++) {
    foo();
  }

  assert(call_count == 100);

  instrumentor->deactivateFunction("_Z3foov");

  for (int i=0; i<100; i++) {
    foo();
  }

  instrumentor->activateFunction("_Z3foov");

  for (int i=0; i<100; i++) {
    foo();
  }

  assert(call_count == 200);

  delete instrumentor;

  exit(0);

}
