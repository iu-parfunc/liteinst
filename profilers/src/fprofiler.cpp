
#include <cstdio>
#include "fprofiler.hpp"

using namespace std;

// Instrumentation Functions
void fb_prologFunction(uint16_t funcId) {

  // fprintf(stderr, "stat address at fb_prologFunction %p\n", stats);
  
  /*
  if (funcId == 1) {
    fprintf(stderr, "At function %d prolog..\n", funcId);
  }
  */

  Fstats* g_stats = (Fstats*) stats;
  if(g_stats->find(funcId) == g_stats->end()) {
    fprintf(stderr, "Initializing function %d statistics..\n", funcId);
    Fstat* stat = new Fstat;
    stat->funcId = funcId;
    stat->count = 1;
    g_stats->insert(make_pair(funcId, stat));
  } else {
    // fprintf(stderr, "At function %d prolog..\n", funcId);
    Fstat* stat = g_stats->find(funcId)->second;
    stat->count++;

    if (stat->count >= 50000) {
      PROFILER_INSTANCE->deactivateFunction(&funcId);
    }
  }

}

void fb_epilogFunction(uint16_t funcId) {
  
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
void FBprofiler::initialize() {

  Profiler::init_internal(fb_prologFunction, fb_epilogFunction);
  statistics = new Fstats;

  // Leaking the reference to the global variable so that 
  // instrumentaion functions can access it without going through object reference
  stats = statistics; 
  
  // fprintf(stderr, "Stats address %p\n", stats);

}

void FBprofiler::dumpStatistics() {

  FILE* fp = fopen("prof.out", "a");

  for(auto iter = statistics->begin(); iter != statistics->end(); iter++) {
    fprintf(fp, "Function id : %d Count %lu\n", iter->first, iter->second->count);
  }

  fclose(fp);

}

FBprofiler::~FBprofiler() {
  fprintf(stderr, "At FBprofier destructor\n");
  dumpStatistics();
  Profiler::cleanup_internal();
  delete (Fstats*)stats;
}
