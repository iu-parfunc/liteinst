
#include <cstdio>
#include "sprofiler.hpp"
#include <pthread.h>
#include <time.h>

using namespace std;

int lock = 0;

TLSSamplingProfilerStats* sampling_thread_stats;

// Instrumentation Functions
void samplingPrologFunction(uint16_t func_id) {

  /*
  __thread static bool allocated;

  if (!allocated) {
    sampling_thread_stats = new TLSSamplingProfilerStats;
    allocated = true;

    ((SamplingProfiler*)PROFILER_INSTANCE)->registerThreadStatistics(sampling_thread_stats);
  }
  */


  // fprintf(stderr, "stat address at fb_prologFunction %p\n", stats);
  
  /*
  if (funcId == 1) {
    fprintf(stderr, "At function %d prolog..\n", funcId);
  }
  */


  SamplingProfilerStats* g_stats = (SamplingProfilerStats*) stats;

  /*
  if (funcId == 88) {
    fprintf(stderr, "Checking function 88\n");
    if (g_stats->find(funcId) == g_stats->end());
    return;
  }
  */

  if(g_stats->find(func_id) == g_stats->end()) {
    fprintf(stderr, "Initializing function %d statistics..\n", func_id);
    SamplingProfilerStat* stat = new SamplingProfilerStat;
    stat->func_id = func_id;
    stat->count = 1;
    stat->count_at_last_activation = 0;
    stat->deactivation_count = 0;
    g_stats->insert(make_pair(func_id, stat));
  } else {
    // fprintf(stderr, "At function %d prolog..\n", funcId);
    SamplingProfilerStat* stat = g_stats->find(func_id)->second;
    stat->count++;

    uint64_t new_count = stat->count - stat->count_at_last_activation; 
    if (new_count >= 10000) {
      
      // Aquire lock
      /*
      while(!__sync_bool_compare_and_swap(&lock, 0 , 1)) {
        fprintf(stderr, "waiting to aquire lock at prolog..\n");
      }
      */

      // fprintf(stderr, "Deactivating function %d\n", stat->func_id);
      PROFILER_INSTANCE->deactivateFunction(&func_id);
      stat->deactivation_count++;
      stat->is_active = false;

      // Release lock
      // __sync_bool_compare_and_swap(&lock, 1 , 0);

      /*
      for(auto iter = g_stats->begin(); iter != g_stats->end(); iter++) {
        Sstat* funcStat = iter->second;

        if (!funcStat->isActive && funcStat->funcId != funcId) {

          // Acquire lock
          // while(__sync_bool_compare_and_swap(&lock, 0 , 1));

          // fprintf(stderr, "Reactivating function %d\n", funcStat->funcId);
          funcStat->count_at_last_activation = funcStat->count;
          PROFILER_INSTANCE->activateFunction(&(funcStat->funcId));
          funcStat->isActive = true;
        }
      }
      */

    }
  }

}

void samplingEpilogFunction(uint16_t func_id) {
  
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

// Probe monitor
void* samplingProbeMonitor(void* param) {

  SamplingProfilerStats* g_stats = (SamplingProfilerStats*) stats;
  struct timespec ts;
  ts.tv_sec = 0;
  ts.tv_nsec = 1000000;

  while(true) {
    for(auto iter = g_stats->begin(); iter != g_stats->end(); iter++) {
      SamplingProfilerStat* func_stat = iter->second;

      if (!func_stat->is_active) {

        // Acquire lock
        /*
        while(!__sync_bool_compare_and_swap(&lock, 0 , 1)) {
          fprintf(stderr, "waiting to aquire lock at sampler..\n");
        }
        */

        // fprintf(stderr, "Reactivating function %d\n", func_stat->func_id);
        func_stat->count_at_last_activation = func_stat->count;
        PROFILER_INSTANCE->activateFunction(&(func_stat->func_id));
        func_stat->is_active = true;
      
        // Release lock
        // __sync_bool_compare_and_swap(&lock, 1 , 0);
      }

      nanosleep(&ts, NULL);
    }
  }

}

// Profiler implementation 
void SamplingProfiler::initialize() {

  Profiler::initInstrumentor(samplingPrologFunction, samplingEpilogFunction);
  statistics = new SamplingProfilerStats;

  // Leaking the reference to the global variable so that 
  // instrumentaion functions can access it without going through object reference
  stats = statistics; 

  spawnMonitor();
  
  // fprintf(stderr, "Stats address %p\n", stats);

}

void SamplingProfiler::spawnMonitor() {
  pthread_t tid;
  pthread_create(&tid, NULL, samplingProbeMonitor, (void*)NULL);
}

void SamplingProfiler::dumpStatistics() {

  FILE* fp = fopen("prof.out", "a");

  for(auto iter = statistics->begin(); iter != statistics->end(); iter++) {
    fprintf(fp, "Function id : %d Count %lu Deactivation Count %d\n", iter->first, iter->second->count, 
        iter->second->deactivation_count);
  }

  fclose(fp);

}

SamplingProfiler::~SamplingProfiler() {
  fprintf(stderr, "At Sprofier destructor\n");
  dumpStatistics();
  Profiler::cleanupInstrumentor();
  delete (SamplingProfilerStats*)stats;
}
