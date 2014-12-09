
#include <cstdio>
#include "fprofiler.hpp"
#include <stdlib.h>

using namespace std;

/* Globals for this profiler */
uint64_t fb_sample_size;


// Thread local statistics table
__thread static TLSBackoffProfilerStat* sampling_thread_stats;
__thread static int thread_id;

// Instrumentation Functions
void backoffPrologFunction(uint16_t func_id) {

  __thread static bool allocated;

  if (!allocated) {
    allocated = true;
    // C++ value initilization.. Similar to calloc
    sampling_thread_stats = new TLSBackoffProfilerStat[INSTRUMENTOR_INSTANCE->getFunctionCount()](); 

    thread_id = ((BackoffProfiler*)PROFILER_INSTANCE)->registerThreadStatistics(sampling_thread_stats);
  }

  sampling_thread_stats[func_id].start_timestamp = getticks();


  /*
  BackoffProfilerStat* g_stats = (BackoffProfilerStat*) g_ubiprof_stats;
  g_stats[func_id].count = 1;
  */

  /*
  if(g_stats->find(func_id) == g_stats->end()) {
    BackoffProfilerStat* stat = new BackoffProfilerStat;
    stat->func_id = func_id;
    stat->count = 1;
    stat->lock = 0;
    g_stats->insert(make_pair(func_id, stat));
  } 
  */

  /*
  if (sampling_thread_stats->find(func_id) == sampling_thread_stats->end()) {
    TLSBackoffProfilerStat* stat = new TLSBackoffProfilerStat;
    stat->func_id = func_id;
    stat->count = 1;
    stat->total_time = 0;
    sampling_thread_stats->insert(make_pair(func_id, stat));
    stat->start_timestamp = getticks();
  } else {
    TLSBackoffProfilerStat* stat = sampling_thread_stats->find(func_id)->second;
    stat->start_timestamp = getticks();
  }
  */

}

void backoffEpilogFunction(uint16_t func_id) {
  
  ticks elapsed = getticks() - sampling_thread_stats[func_id].start_timestamp;
  sampling_thread_stats[func_id].total_time += elapsed;

  BackoffProfilerStat* g_stats = (BackoffProfilerStat*) g_ubiprof_stats;

  int thread_count = ((BackoffProfiler*)PROFILER_INSTANCE)->getThreadCount();
  TLSBackoffProfilerStat** tls_stats = ((BackoffProfiler*)PROFILER_INSTANCE)->getThreadStatistics();

  uint64_t global_count = 0;
  // TLSBackoffProfilerStat* stat = sampling_thread_stats->find(func_id)->second;
  sampling_thread_stats[func_id].count++;

  for (int i=0; i < thread_count; i++) {
    global_count += tls_stats[i][func_id].count;
  }

  if (global_count >= fb_sample_size) {
    if (__sync_bool_compare_and_swap(&(g_stats[func_id].lock), 0 , 1)) {
      PROFILER_INSTANCE->deactivateFunction(&func_id);
      __sync_bool_compare_and_swap(&(g_stats[func_id].lock), 1 , 0);
    }
  }

}

// Profiler implementation 
void BackoffProfiler::initialize() {

  Profiler::initInstrumentor(backoffPrologFunction, backoffEpilogFunction);
  // statistics = new BackoffProfilerStats;
  statistics = new BackoffProfilerStat[ins->getFunctionCount()](); // C++ value initilization.. Similar to calloc

  // Leaking the reference to the global variable so that 
  // instrumentaion functions can access it without going through object reference
  g_ubiprof_stats = statistics; 

  tls_stats = new TLSBackoffProfilerStat*[64](); // C++ value initialization.. Similar to calloc

  char* sample_size_str = getenv("SAMPLE_SIZE");
  if (sample_size_str != NULL) {
    uint64_t size = atol(sample_size_str);
    if (size != 0) {
      fb_sample_size = size;
    } else {
      fb_sample_size = sample_size;
    }
  } else {
    fb_sample_size = sample_size;
  }

  fprintf(stderr, "[Backoff Profiler] **** Parameters : Sample size => %lu\n", fb_sample_size); 

}

int BackoffProfiler::registerThreadStatistics(TLSBackoffProfilerStat* stats) {

  if (thread_counter + 1 < 64) {
    tls_stats[thread_counter++] = stats;
    return thread_counter;
  } else {
    fprintf(stderr, "[Backoff Profiler] Max thread count exceeded. This thread will not be profiled..\n");
    return -1;
  }
}

int BackoffProfiler::getThreadCount() {
  return thread_counter;
}

TLSBackoffProfilerStat** BackoffProfiler::getThreadStatistics() {
  return tls_stats;
}

void BackoffProfiler::dumpStatistics() {

  FILE* fp = fopen("prof.out", "a");

  uint64_t total_count = 0;
  int func_count = ins->getFunctionCount();
  fprintf(stderr, "\nTotal function count : %d\n", func_count);
  fprintf(stderr, "Thread count : %d\n", thread_counter);
  for(int i = 0; i < func_count; i++) {
    statistics[i].count = 0;
    statistics[i].total_time = 0;

    for (int j = 0; j < thread_counter; j++) {
      TLSBackoffProfilerStat* tls_stat = tls_stats[j];
      statistics[i].count += tls_stat[i].count;
      statistics[i].total_time += tls_stat[i].total_time;
    }

    total_count += statistics[i].count;

    if (statistics[i].count != 0) {
      fprintf(fp, "Function name : %s Count %lu Avg time (cycles) : %lu\n", 
          ins->getFunctionName(i).c_str(), statistics[i].count, statistics[i].total_time / statistics[i].count);
    }
  }

  // fprintf(fp, "\n CALLED_FUNCTIONS : %lu\n", total_count);
  fprintf(stderr, "\n CALLED_FUNCTIONS: %lu\n", total_count);

  fclose(fp);

}

BackoffProfiler::~BackoffProfiler() {
  dumpStatistics();
  Profiler::cleanupInstrumentor();
  delete (BackoffProfilerStat*)g_ubiprof_stats;

  for (int i = 0; i < 64; i++) {
    delete tls_stats[i];
  }

  delete tls_stats;
}
