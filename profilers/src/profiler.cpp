
#include <cstdio>
#include "profiler.hpp"
#include "fprofiler.hpp"

#define FINSTRUMENT 0

Profiler* PROFILER_INSTANCE = 0;
void* stats = 0;

Profiler* Profiler::getInstance(int type) {

  if(!PROFILER_INSTANCE) {
    PROFILER_INSTANCE = new FBprofiler();
    PROFILER_INSTANCE->initialize();
  }

  return PROFILER_INSTANCE;

}

void Profiler::init_internal(InstrumentationFunc prologFunction, InstrumentationFunc epilogFunction) {

  ins = Instrumentor::getInstance(FINSTRUMENT, prologFunction, epilogFunction);

}

void Profiler::cleanup_internal() {
  delete ins;
}

int Profiler::activateFunction(void* id) {

  return ins->activateProbe(id, FUNC);

}

int Profiler::deactivateFunction(void* id) {

  return ins->deactivateProbe(id, FUNC);

}

void Profiler::startProfiler() {

}

void Profiler::stopProfiler() {

}

Profiler::~Profiler() {
  // delete ins;
}
