
#include <cstdio>
#include "eprofiler.hpp"

// Thread local statistics data for current thread
static __thread TLStatistics* tl_stat;

TLStatistics* emptyPrologFunction(uint16_t func_id) {
  static __thread bool allocated;

  if (!allocated) {
    allocated = true;
    // C++ value initilization.. Similar to calloc
    tl_stat = new TLStatistics;
    tl_stat->thread_local_overhead = 0;
    tl_stat->thread_local_count = 0;
  }

  return tl_stat;

  // Do nothing
}

TLStatistics* emptyEpilogFunction(uint16_t func_id) {
  return tl_stat;
  // Do nothing
}

void EmptyProfiler::initialize() {
  Profiler::initInstrumentor(emptyPrologFunction, emptyEpilogFunction);
  fprintf(stderr, "[Empty Profiler] **** Parameters : None\n" );
}

void EmptyProfiler::dumpStatistics() {
  // Do nothing
}

EmptyProfiler::~EmptyProfiler() {
  Profiler::cleanupInstrumentor();
}
