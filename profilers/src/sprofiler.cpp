
#include "sprofiler.hpp"

#include <cstdio>
#include <pthread.h>
#include <time.h>
#include <stdlib.h>

using namespace std;

/* Globals for this profiler */
uint64_t sp_sample_size;
uint64_t sp_epoch_period;

// Thread local statistics table 
__thread static TLSSamplingProfilerStats* sampling_thread_stats;

// Instrumentation Functions
void samplingPrologFunction(uint16_t func_id) {

  __thread static bool allocated;

  if (!allocated) {
    sampling_thread_stats = new TLSSamplingProfilerStats;
    allocated = true;

    ((SamplingProfiler*)PROFILER_INSTANCE)->registerThreadStatistics(sampling_thread_stats);
  }

  SamplingProfilerStats* g_stats = (SamplingProfilerStats*) stats;

  if (g_stats->find(func_id) == g_stats->end()) {
    SamplingProfilerStat* stat = new SamplingProfilerStat;
    stat->func_id = func_id;
    stat->count = 1;
    stat->count_at_last_activation = 0;
    stat->deactivation_count = 0;
    stat->lock = 0;
    g_stats->insert(make_pair(func_id, stat));
  }

  if (sampling_thread_stats->find(func_id) == sampling_thread_stats->end()) {
    TLSSamplingProfilerStat* stat = new TLSSamplingProfilerStat;
    stat->func_id = func_id;
    stat->count = 1;
    stat->total_time = 0;
    stat->count_at_last_activation = 0;
    stat->deactivation_count = 0;
    stat->start_timestamp = getticks();
    sampling_thread_stats->insert(make_pair(func_id, stat));
  } else {
    TLSSamplingProfilerStat* stat = sampling_thread_stats->find(func_id)->second;
    stat->start_timestamp = getticks();
  }

}

void samplingEpilogFunction(uint16_t func_id) {

  SamplingProfilerStats* g_stats = (SamplingProfilerStats*) stats;
  SamplingProfilerStat* g_stat = g_stats->find(func_id)->second;
  
  TLSSamplingProfilerStat* tls_stat = sampling_thread_stats->find(func_id)->second;

  int thread_count = ((SamplingProfiler*)PROFILER_INSTANCE)->getThreadCount(); 
  TLSSamplingProfilerStats** tls_stats = ((SamplingProfiler*)PROFILER_INSTANCE)->getThreadStatistics();

  // We do this at epilog itself to get an accurate count than a periodically accumilated count by the probe monitor thread
  uint64_t global_count = 0;
  TLSSamplingProfilerStat* stat = sampling_thread_stats->find(func_id)->second;
  stat->count++;

  for (int i=0; i < thread_count; i++) {
    global_count += tls_stats[i]->find(func_id)->second->count; 
  }

  uint64_t new_count = global_count - g_stat->count_at_last_activation; 

  ticks elapsed = getticks() - tls_stat->start_timestamp;
  tls_stat->total_time += elapsed;
  if (new_count >= sp_sample_size) {
    if (__sync_bool_compare_and_swap(&(g_stat->lock), 0 , 1)) {
      PROFILER_INSTANCE->deactivateFunction(&func_id);
      g_stat->count_at_last_activation = global_count;
      g_stat->deactivation_count++;
      g_stat->is_active = false;
      __sync_bool_compare_and_swap(&(g_stat->lock), 1 , 0);
    }
  }

}

// Probe monitor
void* samplingProbeMonitor(void* param) {

  SamplingProfilerStats* g_stats = (SamplingProfilerStats*) stats;

  uint64_t nanos = sp_epoch_period * 1000000; 
  uint64_t secs = nanos / 1000000000;
  uint64_t nsecs = nanos % 1000000000;

  struct timespec ts;
  ts.tv_sec = secs;
  ts.tv_nsec = nsecs;

  while(true) {
    for(auto iter = g_stats->begin(); iter != g_stats->end(); iter++) {
      SamplingProfilerStat* func_stat = iter->second;

      if (!func_stat->is_active) {

        PROFILER_INSTANCE->activateFunction(&(func_stat->func_id));
        func_stat->is_active = true;
      
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
  // tls_stats = calloc(64, sizeof(TLSSamplingProfilerStats*));
  tls_stats = new TLSSamplingProfilerStats*[64]; 

  char* sample_size_str = getenv("SAMPLE_SIZE");
  if (sample_size_str != NULL) {
    uint64_t size = atol(sample_size_str);
    if (size != 0) {
      sp_sample_size = size;
    } else {
      sp_sample_size = sample_size;
    }
  } else {
    sp_sample_size = sample_size;
  }

  char* epoch_period_str = getenv("EPOCH_PERIOD");
  if (epoch_period_str != NULL) {
    uint64_t period = atol(epoch_period_str);
    if (period != 0) {
      sp_epoch_period = period;
    } else {
      sp_epoch_period = epoch_period;
    }
  } else {
    sp_epoch_period = epoch_period;
  }

  fprintf(stderr, "[Sampling Profiler] **** Parameters : Sample size => %lu Epoch period => %lu\n", sp_sample_size, sp_epoch_period); 

  spawnMonitor();
  
  // fprintf(stderr, "Stats address %p\n", stats);

}

void SamplingProfiler::spawnMonitor() {

  pthread_t tid;
  pthread_create(&tid, NULL, samplingProbeMonitor, (void*)NULL);

}

void SamplingProfiler::registerThreadStatistics(TLSSamplingProfilerStats* stats) {

  if (thread_counter+ 1 < 64) {
    tls_stats[thread_counter++] = stats;
  } else {
    fprintf(stderr, "[Sampling Profiler] Max thread count exceeded. This thread will not be profiled..\n");
  }

}

int SamplingProfiler::getThreadCount() {
  return thread_counter;
}

TLSSamplingProfilerStats** SamplingProfiler::getThreadStatistics() {
  return tls_stats;
}

void SamplingProfiler::dumpStatistics() {

  fprintf(stderr, "[Sampling Profiler] Thread count : %d\n", thread_counter);

  FILE* fp = fopen("prof.out", "a");

  for (auto iter = statistics->begin(); iter != statistics->end(); iter++) {
    SamplingProfilerStat* g_stat = iter->second;
    int func_id = g_stat->func_id;
    g_stat->count = 0;
    g_stat->total_time = 0;

    for (int i = 0; i < thread_counter; i++) {
      TLSSamplingProfilerStats* tls_stat = tls_stats[i];
      g_stat->count += tls_stat->find(func_id)->second->count;
      g_stat->total_time += tls_stat->find(func_id)->second->total_time;
    }

    fprintf(fp, "Function id : %d Count %lu Deactivation Count : %d Avg time (cycles) : %lu\n", func_id,  g_stat->count, 
        g_stat->deactivation_count, g_stat->total_time / g_stat->count); 

  }

  fclose(fp);

}

SamplingProfiler::~SamplingProfiler() {
  dumpStatistics();
  Profiler::cleanupInstrumentor();
  delete (SamplingProfilerStats*)stats;
  delete tls_stats;
}
