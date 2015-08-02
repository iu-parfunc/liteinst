
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

Profiler* Profiler::newInstance(int type) {

  if (PROFILER_INSTANCE) {
    fprintf(stderr, "[Ubiprof] WARNING : deleting the existing profiler possibly with accumilated data..\n");
    delete PROFILER_INSTANCE;
  }

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
  } else if (type == MINIMAL_ADAPTIVE) {
    PROFILER_INSTANCE = new MinimalAdaptiveProfiler();
    PROFILER_INSTANCE->initialize();
  } else if (type == MINIMAL_BACKOFF) {
    PROFILER_INSTANCE = new MinimalBackoffProfiler();
    PROFILER_INSTANCE->initialize();
  } else if (type == MINIMAL_SAMPLING) {
    PROFILER_INSTANCE = new MinimalSamplingProfiler();
    PROFILER_INSTANCE->initialize();
  }

  return PROFILER_INSTANCE;

}

void Profiler::initInstrumentor(InstrumentationFunc prologFunction, InstrumentationFunc epilogFunction) {

  ins = Instrumentor::newInstance(FINSTRUMENT, prologFunction, epilogFunction);

}


Profiler* Profiler::getInstance() {
  return PROFILER_INSTANCE;
}


void Profiler::cleanupInstrumentor() {
  delete ins;
}

int Profiler::activateFunction(uint16_t func_id) {

  return ins->activateFunction(func_id);

}

int Profiler::activateFunction(std::string name) {

  return ins->activateFunction(name);

}

int Profiler::deactivateFunction(uint16_t func_id) {
  g_deactivation_count++;
  return ins->deactivateFunction(func_id);

}

int Profiler::deactivateFunction(std::string name) {
  g_deactivation_count++;
  return ins->deactivateFunction(name);

}

void Profiler::startProfiler() {

}

void Profiler::stopProfiler() {

}

Profiler::~Profiler() {
  
#ifndef NDEBUG 
  fprintf(stderr,"[Profiler.cpp] Executing Profiler instance destructor\n");
#endif 
  
  //delete ins;
}
