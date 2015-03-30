
#include <cstdio>
#include "profiler.hpp"
#include "fprofiler.hpp"
#include "sprofiler.hpp"
#include "eprofiler.hpp"
#include "adaptive_profiler.hpp"

#define FINSTRUMENT 0

Profiler* PROFILER_INSTANCE = 0;
void* g_ubiprof_stats = 0;

// Temporarily exposing monitor thread to get thread overhead statististics to 
// finalize
pthread_t g_monitor_thread;
uint64_t g_probe_overheads = 0;
uint64_t g_probe_count = 0;

Profiler* Profiler::getInstance(int type) {

  // TODO: Change to proper constants. Use constants.h
  if(!PROFILER_INSTANCE) {
    if (type == BACKOFF) {
      PROFILER_INSTANCE = new BackoffProfiler();
      PROFILER_INSTANCE->initialize();
    } else if (type == SAMPLING) {
      PROFILER_INSTANCE = new SamplingProfiler();
      PROFILER_INSTANCE->initialize();
    } else if (type == EMPTY) {
      PROFILER_INSTANCE = new EmptyProfiler();
      PROFILER_INSTANCE->initialize();
    } else if (type == ADAPTIVE) {
      PROFILER_INSTANCE = new AdaptiveProfiler();
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
  g_deactivation_count++;
  return ins->deactivateProbe(id, FUNC);

}

int Profiler::deactivateFunctionByName(void* id) {
  g_deactivation_count++;
  return ins->deactivateProbeByName(id, FUNC);

}

void Profiler::startProfiler() {

}

void Profiler::stopProfiler() {

}

Profiler::~Profiler() {
  // delete ins;
}
