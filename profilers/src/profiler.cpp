
#include <cstdio>
#include "profiler.hpp"
#include "fprofiler.hpp"
#include "sprofiler.hpp"

#define FINSTRUMENT 0

Profiler* PROFILER_INSTANCE = 0;
void* stats = 0;

Profiler* Profiler::getInstance(int type) {

  if(!PROFILER_INSTANCE) {
    if (type == 0) {
      PROFILER_INSTANCE = new BackoffProfiler();
      PROFILER_INSTANCE->initialize();
    } else if (type == 1) {
      PROFILER_INSTANCE = new SamplingProfiler();
      PROFILER_INSTANCE->initialize();
    }
  }

  return PROFILER_INSTANCE;

}

void Profiler::initInstrumentor(InstrumentationFunc prologFunction, InstrumentationFunc epilogFunction) {

  ins = Instrumentor::getInstance(FINSTRUMENT, prologFunction, epilogFunction);

}

void Profiler::cleanupInstrumentor() {
  delete ins;
}

int Profiler::activateFunction(void* id) {

  return ins->activateProbe(id, FUNC);

}

int Profiler::activateFunctionByName(void* id) {

  return ins->activateProbeByName(id, FUNC);

}


int Profiler::deactivateFunction(void* id) {

  return ins->deactivateProbe(id, FUNC);

}

int Profiler::deactivateFunctionByName(void* id) {

  return ins->deactivateProbeByName(id, FUNC);

}

void Profiler::startProfiler() {

}

void Profiler::stopProfiler() {

}

Profiler::~Profiler() {
  // delete ins;
}
