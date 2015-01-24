
#include "sprofiler.hpp"

#include <cstdio>
#include <pthread.h>
#include <time.h>
#include <stdlib.h>

using namespace std;

/* Globals for this profiler */
uint64_t sp_sample_size;
uint64_t sp_epoch_period;
uint64_t total_overhead = 0; // Overhead incurred due to profiling
uint64_t total_process_time = 0; // Total process time until last epoch sample
uint64_t last_epoch_random = 0; // Random added to last epoch period

// Thread local statistics table 
__thread static TLSSamplingProfilerStat* sampling_thread_stats;

// Thread local overhead counter
__thread static uint64_t thread_local_overhead = 0;

// Instrumentation Functions
void samplingPrologFunction(uint16_t func_id) {

  ticks prolog_start = getticks();
  __thread static bool allocated;

  if (!allocated) {
    allocated = true;
    // C++ value initilization.. Similar to calloc
    sampling_thread_stats = new TLSSamplingProfilerStat[INSTRUMENTOR_INSTANCE->getFunctionCount()]();

    ((SamplingProfiler*)PROFILER_INSTANCE)->registerThreadStatistics(sampling_thread_stats);
  }

  sampling_thread_stats[func_id].start_timestamp = getticks();
  thread_local_overhead += (sampling_thread_stats[func_id].start_timestamp - prolog_start);

  /*
  SamplingProfilerStat* g_stats = (SamplingProfilerStat*) g_ubiprof_stats;

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
  
  */

}

void samplingEpilogFunction(uint16_t func_id) {

  ticks epilog_start = getticks();

  SamplingProfilerStat* g_stats = (SamplingProfilerStat*) g_ubiprof_stats;
  // SamplingProfilerStat* g_stat = g_stats->find(func_id)->second;
  
  // TLSSamplingProfilerStat* tls_stat = sampling_thread_stats->find(func_id)->second;

  int thread_count = ((SamplingProfiler*)PROFILER_INSTANCE)->getThreadCount(); 
  TLSSamplingProfilerStat** tls_stats = ((SamplingProfiler*)PROFILER_INSTANCE)->getThreadStatistics();

  // We do this at epilog itself to get an accurate count than a periodically accumilated count by the probe monitor thread
  uint64_t global_count = 0;
  // TLSSamplingProfilerStat* stat = sampling_thread_stats->find(func_id)->second;
  sampling_thread_stats[func_id].count++;

  for (int i=0; i < thread_count; i++) {
    global_count += tls_stats[i][func_id].count; 
  }

  uint64_t new_count = global_count - g_stats[func_id].count_at_last_activation; 

  ticks elapsed = epilog_start - sampling_thread_stats[func_id].start_timestamp;
  sampling_thread_stats[func_id].total_time += elapsed;
  if (new_count >= sp_sample_size) {
    if (__sync_bool_compare_and_swap(&(g_stats[func_id].lock), 0 , 1)) {
      PROFILER_INSTANCE->deactivateFunction(&func_id);
      g_stats[func_id].count_at_last_activation = global_count;
      g_stats[func_id].deactivation_count++;
      g_stats[func_id].is_active = false;
      __sync_bool_compare_and_swap(&(g_stats[func_id].lock), 1 , 0);
    }
  }

  ticks epilog_end = getticks();
  thread_local_overhead += (epilog_end - epilog_start);

}

// Probe monitor
void* samplingProbeMonitor(void* param) {

  SamplingProfilerStat* g_stats = (SamplingProfilerStat*) g_ubiprof_stats;

  uint64_t nanos = sp_epoch_period * 1000000; 
  uint64_t secs = nanos / 1000000000;
  uint64_t nsecs = nanos % 1000000000;

  struct timespec ts;
  ts.tv_sec = secs;
  ts.tv_nsec = nsecs;

  while(true) {
    int func_count = INSTRUMENTOR_INSTANCE->getFunctionCount();
    for(int i = 0; i < func_count; i++) {
      if (!g_stats[i].is_active) {
        PROFILER_INSTANCE->activateFunction(&i);
        g_stats[i].is_active = true;
      }
    }

    nanosleep(&ts, NULL);

    /*
    for(auto iter = g_stats->begin(); iter != g_stats->end(); iter++) {
      SamplingProfilerStat* func_stat = iter->second;

      if (!func_stat->is_active) {

        PROFILER_INSTANCE->activateFunction(&(func_stat->func_id));
        func_stat->is_active = true;
      
      }

      nanosleep(&ts, NULL);
    }
    */
  }
}

// Profiler implementation 
void SamplingProfiler::initialize() {

  Profiler::initInstrumentor(samplingPrologFunction, samplingEpilogFunction);

  int func_count = ins->getFunctionCount();
  statistics = new SamplingProfilerStat[func_count](); // C++ value initialization. Similar to calloc

  for (int i=0; i < func_count; i++) {
    statistics[i].is_active = true;
  }

  // Leaking the reference to the global variable so that 
  // instrumentaion functions can access it without going through object reference
  g_ubiprof_stats = statistics; 
  // tls_stats = calloc(64, sizeof(TLSSamplingProfilerStats*));
  tls_stats = new TLSSamplingProfilerStat*[64](); // C++ value initialization. Similar to calloc 

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

void SamplingProfiler::registerThreadStatistics(TLSSamplingProfilerStat* stats) {

  if (thread_counter+ 1 < 64) {
    tls_stats[thread_counter++] = stats;
  } else {
    fprintf(stderr, "[Sampling Profiler] Max thread count exceeded. This thread will not be profiled..\n");
  }

}

int SamplingProfiler::getThreadCount() {
  return thread_counter;
}

TLSSamplingProfilerStat** SamplingProfiler::getThreadStatistics() {
  return tls_stats;
}

void SamplingProfiler::dumpStatistics() {

  fprintf(stderr, "[Sampling Profiler] Thread count : %d\n", thread_counter);

  FILE* fp = fopen("prof.out", "a");

  uint64_t total_count = 0;
  int func_count = ins->getFunctionCount();
  fprintf(stderr, "\nTotal function count : %d\n", func_count);
  fprintf(stderr, "Thread count : %d\n", thread_counter);
  for(int i=0; i < func_count; i++) {
    statistics[i].count = 0;
    statistics[i].total_time = 0;

    for(int j=0; j < thread_counter; j++) {
      TLSSamplingProfilerStat* tls_stat = tls_stats[j];
      statistics[i].count += tls_stat[i].count;
      statistics[i].total_time += tls_stat[i].total_time;
    }

    total_count += statistics[i].count;

    if (statistics[i].count != 0) {
      fprintf(fp, "Function Name: %s Count %lu Deactivation Count : %d Avg time (cycles) : %lu\n", 
          ins->getFunctionName(i).c_str(),  statistics[i].count, 
          statistics[i].deactivation_count, statistics[i].total_time / statistics[i].count); 
    }
  }

  fprintf(stderr, "\n CALLED_FUNCTIONS: %lu\n", total_count);

  fclose(fp);

}

SamplingProfiler::~SamplingProfiler() {
  dumpStatistics();
  Profiler::cleanupInstrumentor();
  delete (SamplingProfilerStat*)g_ubiprof_stats;

  for (int i = 0; i < 64; i++) {
    delete tls_stats[i];
  }

  delete tls_stats;
}
