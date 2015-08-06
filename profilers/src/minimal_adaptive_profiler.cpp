
#include <cstdio>
#include <pthread.h>
#include <time.h>
#include <sys/time.h>
#include <stdlib.h>
#include <string>
#include <string.h>
#include <list>
#include <limits.h>
#include <assert.h>

#include "adaptive_profiler.hpp"
#include "overhead_series.hpp"

using namespace std;

// All thread statistics data thread local reference
static __thread TLStatistics** all_thread_stats;

// Thread statistics data for current thread
static __thread TLStatistics* current_thread_stats;

// Function statistics table for current thread 
static __thread TLSAdaptiveProfilerStat* current_thread_func_stats_table;

static __thread bool allocated=false;

//TLStatistics** MinimalAdaptiveProfiler::getThreadStatistics() {
//  fprintf(stderr,"USING MINIMAL getThreadStatistics\n");
//  return tls_stats;
//}


// Instrumentation Functions
TLStatistics* minimalAdaptivePrologFunction(uint16_t func_id) {

  if (!allocated) {
    allocated = true;
    long function_count = INSTRUMENTOR_INSTANCE->getFunctionCount();
    // C++ value initilization.. Similar to calloc
    // int* intArr = new int[10];
    current_thread_func_stats_table = new TLSAdaptiveProfilerStat[function_count]();
    assert(current_thread_func_stats_table != NULL);
    current_thread_stats = new TLStatistics;
    current_thread_stats->func_stats = current_thread_func_stats_table;
    current_thread_stats->thread_local_overhead = 0;
    current_thread_stats->thread_local_count = 0;

    ((AdaptiveProfiler*)PROFILER_INSTANCE)->registerThreadStatistics(current_thread_stats);
    all_thread_stats = ((AdaptiveProfiler*)PROFILER_INSTANCE)->getThreadStatistics();

#ifndef NDEBUG
    pthread_t tid = pthread_self();
    fprintf(stderr,"ThreadID: %lu has allocated local storage at: 0x%p\n", (unsigned long int)tid,(void*)current_thread_stats);
#endif 
  }

  TLSAdaptiveProfilerStat* local_func_stats = &current_thread_func_stats_table[func_id];

  local_func_stats->invocation_stack[0].func_id = func_id;
  local_func_stats->invocation_stack[0].timestamp = getticks();

  current_thread_stats->thread_local_count++;

  return current_thread_stats;

}

TLStatistics* minimalAdaptiveEpilogFunction(uint16_t func_id) {

  // Somehow epilog got called without proper prolog execution. (e.g: Race
  // condition on deactivation etc). Just return in that case.

  long function_count = INSTRUMENTOR_INSTANCE->getFunctionCount();

  // BJS: Need to return something and check for error in caller. 

  if (!allocated) {
#ifndef NDEBUG
    pthread_t tid = pthread_self();
    fprintf(stderr,"ThreadID %lu \"allocated=false\" in EpilogFunction \n", (unsigned long int)tid);
#endif 
    return NULL;
  }

  ticks epilog_start = getticks();

  
  
  AdaptiveProfilerStat* global_func_stats = &((AdaptiveProfilerStat*) g_ubiprof_stats)[func_id];
  TLSAdaptiveProfilerStat* local_func_stats = &current_thread_func_stats_table[func_id];

  int thread_count = ((AdaptiveProfiler*)PROFILER_INSTANCE)->getThreadCount(); 


  InvocationData i_data = local_func_stats->invocation_stack[0];

  local_func_stats->count++;

  // We do this at epilog itself to get an accurate count than a periodically accumilated count by the probe monitor thread
  uint64_t global_count = 0;

  for (int i=0; i < thread_count; i++) {
    global_count += ((TLSAdaptiveProfilerStat*) all_thread_stats[i]->func_stats)[func_id].count; 
  }

  //uint64_t old_count = global_func_stats->count_at_last_activation; 
  uint64_t new_count = global_count - global_func_stats->count_at_last_activation; 

  ticks elapsed = epilog_start - i_data.timestamp ;

  if (elapsed < local_func_stats->min_time || local_func_stats->min_time == 0) {
    local_func_stats->min_time = elapsed;
  }

  if (elapsed > local_func_stats->max_time) {
    local_func_stats->max_time = elapsed;
  }

  local_func_stats->total_time += elapsed;
  // There is a chance this condition might be satistified during the calibrations at init time
  // Skip deactivating if that's the case
  if (new_count >= global_func_stats->sample_size && g_ubiprof_initialized) {  
    if (__sync_bool_compare_and_swap(&(global_func_stats->lock), 0 , 1)) {
      if (PROFILER_INSTANCE->deactivateFunction(func_id) != -1) {
    
// #ifndef NDEBUG
// 	pthread_t tid = pthread_self();
// 	uint64_t old_count2 = global_func_stats->count_at_last_activation;
// 	if (old_count != old_count2) {
// 	  fprintf(stderr,"ThreadID %lu: WARNING GLOBAL COUNT WILL BE BROKEN\nold_count=%llu, old_count2=%llu\n", (unsigned long int)tid,old_count,old_count2); 
// 	}
// #endif 
    
    
        global_func_stats->deactivation_count++; // Store in thread local structure and we sum all TL stuff when flushing results
        global_func_stats->count_at_last_activation = global_count;
        global_func_stats->active = false;
      }
      __sync_bool_compare_and_swap(&(global_func_stats->lock), 1 , 0);
    }
  }


  current_thread_stats->thread_local_count++;
  return current_thread_stats;

}

// Profiler implementation 
void MinimalAdaptiveProfiler::initialize() {

  Profiler::initInstrumentor(minimalAdaptivePrologFunction, minimalAdaptiveEpilogFunction);

#if OVERHEAD_TIME_SERIES
  overhead_time_series = new list<string>();
#endif

  long func_count = ins->getFunctionCount();
  fprintf(stderr, "[DEBUG] Adaptive Profiler function count : %ld\n", func_count);
  // statistics = new AdaptiveProfilerStat[func_count](); // C++ value initialization. Similar to calloc
  statistics = (AdaptiveProfilerStat*) calloc(func_count, sizeof(AdaptiveProfilerStat)); 

  // Leaking the reference to the global variable so that 
  // instrumentaion functions can access it without going through object reference
  g_ubiprof_stats = statistics; 
  tls_stats = new TLStatistics*[64](); // C++ value initialization. Similar to calloc 

  char* sample_size_str = getenv("SAMPLE_SIZE");
  if (sample_size_str != NULL) {
    uint64_t size = atol(sample_size_str);
    if (size != 0) {
      sp_sample_size = size;
      sp_initial_sample_size = size;
    } else {
      sp_sample_size = sample_size;
      sp_initial_sample_size = sample_size;
    }
  } else {
    sp_sample_size = sample_size;
    sp_initial_sample_size = sample_size;
  }

  for (int i=0; i < func_count; i++) {
    statistics[i].active = true;
    statistics[i].sample_size = sp_sample_size;
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

  char* target_overhead_str = getenv("TARGET_OVERHEAD");
  if (target_overhead_str != NULL) {
    uint64_t overhead = atol(target_overhead_str);
    if (overhead!= 0) {
      sp_target_overhead = overhead;
    } else {
      sp_target_overhead = target_overhead ;
    }
  } else {
    sp_target_overhead = target_overhead;
    fprintf(stderr, "[DEBUG] Overhead : %.2lf Default Overhead : %.2lf\n", sp_target_overhead, target_overhead);
  }

  char* adaptive_strategy_str = getenv("ADAPTIVE_STRATEGY");
  if (adaptive_strategy_str != NULL) {
    if (!strcmp(adaptive_strategy_str, "SLOW_RAMP_UP")) {
	    g_strategy = SLOW_RAMP_UP;
    } else if (!strcmp(adaptive_strategy_str, "PROPOTIONAL")) {
      g_strategy = PROPOTIONAL;
    } else if (!strcmp(adaptive_strategy_str, "EPOCH_CONTROL")) {
      g_strategy = EPOCH_CONTROL;
    } else {
      g_strategy = SLOW_RAMP_UP;
    } 
  } 

  fprintf(stderr, "[Minimal Adaptive Profiler] **** Parameters : Sample size => %lu Epoch period (ms) => %.2lf Target overhead => %.2lf\n", 
      sp_sample_size, sp_epoch_period, sp_target_overhead); 

  switch(g_strategy) {
    case SLOW_RAMP_UP:
      fprintf(stderr, "[Minimal Adaptive Profiler] Adaptive Strategy : SLOW_RAMP_UP\n");
      break;
    case PROPOTIONAL:
      fprintf(stderr, "[Minimal Adaptive Profiler] Adaptive Strategy : PROPOTIONAL\n");
      break;
    default:
      fprintf(stderr, "[Minimal Adaptive Profiler] Adaptive Strategy : EPOCH_CONTROL\n");

  }

  spawnMonitor();

}

void MinimalAdaptiveProfiler::dumpStatistics() {

  fprintf(stderr, "[MinimalAdaptive Profier] Flushing statistics to disk..\n");

  FILE* fp = fopen("prof.out", "a");

  uint64_t total_count = 0;
  int func_count = ins->getFunctionCount();
  fprintf(stderr, "[finstrumentor] Total function count : %d\n", func_count);
  fprintf(stderr, "[Minimal Adaptive Profiler] Thread count : %d\n", thread_counter);
  fprintf(fp, "Function,Count,Min_time,Max_time,Avg_time,Deactivation_count\n");
  for(int i=0; i < func_count; i++) {
    statistics[i].count = 0;
    statistics[i].total_time = 0;
    // statistics[i].deactivation_count = 0;
    statistics[i].limited_count = 0;

    ticks cur_min = ULLONG_MAX, cur_max = 0;
    for(int j=0; j < thread_counter; j++) {
      TLSAdaptiveProfilerStat* tls_func_stat = (TLSAdaptiveProfilerStat*) tls_stats[j]->func_stats;
      statistics[i].count += tls_func_stat[i].count;
      statistics[i].total_time += tls_func_stat[i].total_time;
      statistics[i].limited_count += tls_func_stat[i].limited_count;
      // statistics[i].deactivation_count += tls_func_stat[i].deactivation_count;

      if (cur_min > tls_func_stat[i].min_time) {
        cur_min = tls_func_stat[i].min_time;
      }

      if (cur_max < tls_func_stat[i].max_time) {
        cur_max = tls_func_stat[i].max_time;
      }
    }

    total_count += statistics[i].count;
    statistics[i].min_time = cur_min;
    statistics[i].max_time = cur_max;
    
    if (statistics[i].count != 0) {
      fprintf(fp, "%s,%lu,%llu,%llu,%llu,%d\n",  
          ins->getFunctionName(i).c_str(),  statistics[i].count,
          statistics[i].min_time, statistics[i].max_time, 
          statistics[i].total_time / (statistics[i].count - statistics[i].limited_count), 
          statistics[i].deactivation_count); 
    }
  }

  fprintf(stderr, "\n[Minimal Adaptive Profiler] NUMBER_OF_FUNCTION_CALLS: %lu\n", total_count);

  fclose(fp);

#ifdef OVERHEAD_TIME_SERIES
  fp = fopen("overhead.out", "a");

  for (list<string>:: iterator it = overhead_time_series->begin(); it != overhead_time_series->end(); ++it) {
    fprintf(fp, "%s\n", (*it).c_str());
  }

  fprintf(fp, ">>>>\n");
  fclose(fp);

  fp = fopen("statistics.out", "a");
  fprintf(fp, "\nNUMBER_OF_FUNCTION_CALLS: %lu\n", total_count);
  fprintf(fp, "SKIPPED_EPOCHS: %lu\n", g_skipped_epochs);
  fprintf(fp, "TOTAL_EPOCHS: %lu\n", g_time_step);
  fclose(fp);

#endif

}

MinimalAdaptiveProfiler::~MinimalAdaptiveProfiler() {

}
