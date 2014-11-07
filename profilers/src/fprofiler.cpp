
#include <cstdio>
#include "fprofiler.hpp"

using namespace std;

// Instrumentation Functions
void backoffPrologFunction(uint16_t func_id) {

  // fprintf(stderr, "stat address at fb_prologFunction %p\n", stats);
  
  /*
  if (funcId == 1) {
    fprintf(stderr, "At function %d prolog..\n", funcId);
  }
  */

  BackoffProfilerStats* g_stats = (BackoffProfilerStats*) stats;

  /*
  if (funcId == 88) {
    fprintf(stderr, "Checking function 88\n");
    if (g_stats->find(funcId) == g_stats->end());
    return;
  }
  */

  if(g_stats->find(func_id) == g_stats->end()) {
    fprintf(stderr, "Initializing function %d statistics..\n", func_id);
    BackoffProfilerStat* stat = new BackoffProfilerStat;
    stat->func_id = func_id;
    stat->count = 1;
    g_stats->insert(make_pair(func_id, stat));
  } else {
    // fprintf(stderr, "At function %d prolog..\n", funcId);
    BackoffProfilerStat* stat = g_stats->find(func_id)->second;
    stat->count++;

    /*
    if (stat->count >= 50000) {
      PROFILER_INSTANCE->deactivateFunction(&funcId);
    }
    */
  }

}

void backoffEpilogFunction(uint16_t func_id) {
  
  /*
  if (funcId == 1) {
    fprintf(stderr, "At function %d epilog..\n", funcId);
  }
  */

  /*
  Fstats* g_stats = (Fstats*) stats;
  Fstat* stat = g_stats->find(funcId)->second;
    
  if (stat->count >= 2) {
      PROFILER_INSTANCE->deactivateFunction(&funcId);
  }
  */

}

// Profiler implementation 
void BackoffProfiler::initialize() {

  Profiler::initInstrumentor(backoffPrologFunction, backoffEpilogFunction);
  statistics = new BackoffProfilerStats;

  // Leaking the reference to the global variable so that 
  // instrumentaion functions can access it without going through object reference
  stats = statistics; 
  
  // fprintf(stderr, "Stats address %p\n", stats);

}

void BackoffProfiler::dumpStatistics() {

  FILE* fp = fopen("prof.out", "a");

  for(auto iter = statistics->begin(); iter != statistics->end(); iter++) {
    fprintf(fp, "Function id : %d Count %lu\n", iter->first, iter->second->count);
  }

  fclose(fp);

}

BackoffProfiler::~BackoffProfiler() {
  fprintf(stderr, "At FBprofier destructor\n");
  dumpStatistics();
  Profiler::cleanupInstrumentor();
  delete (BackoffProfilerStats*)stats;
}
