
#include <cstdio>
#include "eprofiler.hpp"

void emptyPrologFunction(uint16_t func_id) {
  // Do nothing
}

void emptyEpilogFunction(uint16_t func_id) {
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
