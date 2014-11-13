
#include <cstdio>
#include "fprofiler.hpp"
#include <stdlib.h>

using namespace std;

/* Globals for this profiler */
uint64_t fb_sample_size;


// Thread local statistics table
__thread static TLSBackoffProfilerStats* sampling_thread_stats;

// Instrumentation Functions
void backoffPrologFunction(uint16_t func_id) {

  __thread static bool allocated;

  if (!allocated) {
    sampling_thread_stats = new TLSBackoffProfilerStats;
    allocated = true;

    ((BackoffProfiler*)PROFILER_INSTANCE)->registerThreadStatistics(sampling_thread_stats);
  }

  BackoffProfilerStats* g_stats = (BackoffProfilerStats*) stats;

  if(g_stats->find(func_id) == g_stats->end()) {
    BackoffProfilerStat* stat = new BackoffProfilerStat;
    stat->func_id = func_id;
    stat->count = 1;
    stat->lock = 0;
    g_stats->insert(make_pair(func_id, stat));
  }

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

}

void backoffEpilogFunction(uint16_t func_id) {
  
  TLSBackoffProfilerStat* tls_stat = sampling_thread_stats->find(func_id)->second;
  ticks elapsed = getticks() - tls_stat->start_timestamp;
  tls_stat->total_time += elapsed;

  BackoffProfilerStats* g_stats = (BackoffProfilerStats*) stats;
  BackoffProfilerStat* g_stat = g_stats->find(func_id)->second;

  int thread_count = ((BackoffProfiler*)PROFILER_INSTANCE)->getThreadCount();
  TLSBackoffProfilerStats** tls_stats = ((BackoffProfiler*)PROFILER_INSTANCE)->getThreadStatistics();

  uint64_t global_count = 0;
  TLSBackoffProfilerStat* stat = sampling_thread_stats->find(func_id)->second;
  stat->count++;

  for (int i=0; i < thread_count; i++) {
    global_count += tls_stats[i]->find(func_id)->second->count;
  }

  if (global_count >= fb_sample_size) {
    if (__sync_bool_compare_and_swap(&(g_stat->lock), 0 , 1)) {
      PROFILER_INSTANCE->deactivateFunction(&func_id);
      __sync_bool_compare_and_swap(&(g_stat->lock), 1 , 0);
    }
  }

}

// Profiler implementation 
void BackoffProfiler::initialize() {

  Profiler::initInstrumentor(backoffPrologFunction, backoffEpilogFunction);
  statistics = new BackoffProfilerStats;

  // Leaking the reference to the global variable so that 
  // instrumentaion functions can access it without going through object reference
  stats = statistics; 

  tls_stats = new TLSBackoffProfilerStats*[64]; 

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

void BackoffProfiler::registerThreadStatistics(TLSBackoffProfilerStats* stats) {

  if (thread_counter+ 1 < 64) {
    tls_stats[thread_counter++] = stats;
  } else {
    fprintf(stderr, "[Backoff Profiler] Max thread count exceeded. This thread will not be profiled..\n");
  }

}

int BackoffProfiler::getThreadCount() {
  return thread_counter;
}

TLSBackoffProfilerStats** BackoffProfiler::getThreadStatistics() {
  return tls_stats;
}

void BackoffProfiler::dumpStatistics() {

  FILE* fp = fopen("prof.out", "a");

  uint64_t total_count = 0;
  for(auto iter = statistics->begin(); iter != statistics->end(); iter++) {
    BackoffProfilerStat* g_stat = iter->second;
    int func_id = g_stat->func_id;
    g_stat->count = 0;
    g_stat->total_time = 0;

    for (int i = 0; i < thread_counter; i++) {
      TLSBackoffProfilerStats* tls_stat = tls_stats[i];
      g_stat->count += tls_stat->find(func_id)->second->count;
      g_stat->total_time += tls_stat->find(func_id)->second->total_time;
    }

    total_count = g_stat->count;

    fprintf(fp, "Function id : %d Count %lu Avg time (cycles) : %lu\n", 
        g_stat->func_id, g_stat->count, g_stat->total_time / g_stat->count);
  }

  fprintf(fp, "\n CALLED_FUNCTIONS : %lu\n", total_count);
  fprintf(stderr, "\n CALLED_FUNCTIONS : %lu\n", total_count);

  fclose(fp);

}

BackoffProfiler::~BackoffProfiler() {
  dumpStatistics();
  Profiler::cleanupInstrumentor();
  delete (BackoffProfilerStats*)stats;
}
