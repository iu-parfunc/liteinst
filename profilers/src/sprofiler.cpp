
#include <cstdio>
#include <pthread.h>
#include <time.h>
#include <sys/time.h>
#include <stdlib.h>
#include <string>
#include <string.h>
#include <list>
#include <limits.h>

#include "sprofiler.hpp"
#include "overhead_series.hpp"

using namespace std;

// All thread statistics data thread local reference
static __thread TLStatistics** all_thread_stats;

// Thread statistics data for current thread
static __thread TLStatistics* current_thread_stats;

// Function statistics table for current thread 
static __thread TLSSamplingProfilerStat* current_thread_func_stats_table;

// Instrumentation Functions
TLStatistics* samplingPrologFunction(uint16_t func_id) {

  static __thread bool allocated;

  if (!allocated) {
    allocated = true;
    uint32_t function_count = INSTRUMENTOR_INSTANCE->getFunctionCount(); 
    // C++ value initilization.. Similar to calloc
    current_thread_func_stats_table = new TLSSamplingProfilerStat[function_count]();
    current_thread_stats = new TLStatistics;
    current_thread_stats->func_stats = current_thread_func_stats_table;
    current_thread_stats->thread_local_overhead = 0;
    current_thread_stats->thread_local_count = 0;

    for (int i=0; i < function_count; i++) {
      current_thread_func_stats_table[i].is_leaf = true;
      current_thread_func_stats_table[i].stack_depth = 0;
    }

    ((SamplingProfiler*)PROFILER_INSTANCE)->registerThreadStatistics(current_thread_stats);
    all_thread_stats = ((SamplingProfiler*)PROFILER_INSTANCE)->getThreadStatistics();
  }

  SamplingProfilerStat* global_func_stats = &((SamplingProfilerStat*) g_ubiprof_stats)[func_id];
  TLSSamplingProfilerStat* local_func_stats = &current_thread_func_stats_table[func_id];

  uint32_t stack_depth = local_func_stats->stack_depth;
  if (local_func_stats->stack_depth > 0 && 
      local_func_stats->invocation_stack[stack_depth-1].timestamp < global_func_stats->latest_activation_time) {
    local_func_stats->stack_depth = 0;
    local_func_stats->ignore_count = 0;
  } 

  // Limit stack depth of each per each function stack
  if (stack_depth < 20) {
    local_func_stats->invocation_stack[stack_depth].func_id = func_id;
    local_func_stats->invocation_stack[stack_depth].prolog_leaf_count = prolog_leaf_counter;
    local_func_stats->invocation_stack[stack_depth].epilog_leaf_count = epilog_leaf_counter;
    local_func_stats->invocation_stack[stack_depth].timestamp = getticks();
    local_func_stats->stack_depth++;
  } else {
    local_func_stats->ignore_count++; // Ignores this sample since stack limit exceeded
  }

  if (!global_func_stats->active) {
    local_func_stats->stack_depth = 0;
    local_func_stats->ignore_count = 0;
  }

  current_thread_stats->thread_local_count++;
  return current_thread_stats;

}

TLStatistics* samplingEpilogFunction(uint16_t func_id) {

  ticks epilog_start = getticks();

  SamplingProfilerStat* global_func_stats = &((SamplingProfilerStat*) g_ubiprof_stats)[func_id];
  TLSSamplingProfilerStat* local_func_stats = &current_thread_func_stats_table[func_id];

  int thread_count = ((SamplingProfiler*)PROFILER_INSTANCE)->getThreadCount(); 
  uint32_t stack_depth = local_func_stats->stack_depth;

  if (local_func_stats->stack_depth == 0) {
    return current_thread_stats;
  }

  InvocationData i_data = local_func_stats->invocation_stack[stack_depth-1];
  if (i_data.timestamp < global_func_stats->latest_activation_time) {
    local_func_stats->stack_depth = 0;
    local_func_stats->ignore_count = 0;
    return current_thread_stats;
  }

  if (stack_depth >= 20 && local_func_stats->ignore_count > 0) {
    local_func_stats->ignore_count--;
    local_func_stats->limited_count++;
    local_func_stats->count++;

    current_thread_stats->thread_local_count++;
    return current_thread_stats;
  }

  uint64_t prolog_leaf_count_diff = prolog_leaf_counter - i_data.prolog_leaf_count;
  uint64_t epilog_leaf_count_diff = epilog_leaf_counter - i_data.epilog_leaf_count;

  uint64_t leaf_count_diff = 0;
  if (prolog_leaf_count_diff > 0) {
    leaf_count_diff += prolog_leaf_count_diff;
  }

  if (epilog_leaf_count_diff > 0) {
    leaf_count_diff += epilog_leaf_count_diff;
  }

  epilog_leaf_counter++;

  if (leaf_count_diff > 0) {
    local_func_stats->is_leaf = false;
  }

  local_func_stats->count++;

  // We do this at epilog itself to get an accurate count than a periodically accumilated count by the probe monitor thread
  uint64_t global_count = 0;

  for (int i=0; i < thread_count; i++) {
    global_count += ((TLSSamplingProfilerStat*) all_thread_stats[i]->func_stats)[func_id].count; 
  }

  uint64_t new_count = global_count - global_func_stats->count_at_last_activation; 

  ticks elapsed = epilog_start - i_data.timestamp ;

  if (elapsed < local_func_stats->min_time || local_func_stats->min_time == 0) {
    local_func_stats->min_time = elapsed;
  }

  if (elapsed > local_func_stats->max_time) {
    local_func_stats->max_time = elapsed;
  }

  local_func_stats->total_time += elapsed;
  if (new_count >= global_func_stats->sample_size) {
    if (__sync_bool_compare_and_swap(&(global_func_stats->lock), 0 , 1)) {
      PROFILER_INSTANCE->deactivateFunction(&func_id);
      global_func_stats->deactivation_count++; // Store in thread local structure and we sum all TL stuff when flushing results
      global_func_stats->count_at_last_activation = global_count;
      global_func_stats->active = false;
      __sync_bool_compare_and_swap(&(global_func_stats->lock), 1 , 0);
    }
  }

  if (!global_func_stats->active) {
    local_func_stats->stack_depth = 0;
    local_func_stats->ignore_count = 0;
  } else {
    local_func_stats->stack_depth--;
  }

  current_thread_stats->thread_local_count++;
  return current_thread_stats;

}

// Probe monitor
void* samplingProbeMonitor(void* param) {

  SamplingProfilerStat* g_stats = (SamplingProfilerStat*) g_ubiprof_stats;
  int func_count = INSTRUMENTOR_INSTANCE->getFunctionCount();

  struct timespec ts;
  while(true) {
    for(int i = 0; i < func_count; i++) {
      if (!g_stats[i].active) {
        g_stats[i].sample_size = sp_sample_size;
        PROFILER_INSTANCE->activateFunction(&i);
        g_stats[i].active = true;
      } else {
        __sync_bool_compare_and_swap(&g_stats[i].sample_size, g_stats[i].sample_size, sp_sample_size); // Atomically set the value
      }
    } 

    uint64_t nanos = sp_epoch_period * 1000000; 
    uint64_t secs = nanos / 1000000000;
    uint64_t nsecs = nanos % 1000000000;
    ts.tv_sec = secs;
    ts.tv_nsec = nsecs;
    nanosleep(&ts, NULL);

  }
}

// Profiler implementation 
void SamplingProfiler::initialize() {

  Profiler::initInstrumentor(samplingPrologFunction, samplingEpilogFunction);

#if OVERHEAD_TIME_SERIES
  overhead_time_series = new list<string>();
#endif

  int func_count = ins->getFunctionCount();
  statistics = new SamplingProfilerStat[func_count](); // C++ value initialization. Similar to calloc

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
  }

  fprintf(stderr, "[Sampling Profiler] **** Parameters : Sample size => %lu Epoch period => %.2lf Target overhead => %.2lf \n", 
      sp_sample_size, sp_epoch_period, sp_target_overhead); 

  spawnMonitor();

}

void SamplingProfiler::spawnMonitor() {

  pthread_create(&g_monitor_thread, NULL, samplingProbeMonitor, (void*)NULL);

}

void SamplingProfiler::registerThreadStatistics(TLStatistics* stats) {
  // while(__sync_bool_compare_and_swap(&g_thread_lock, 0 , 1)); // Acquire lock

  if (thread_counter+ 1 < 64) {
    tls_stats[thread_counter++] = stats;
  } else {
    fprintf(stderr, "[Sampling Profiler] Max thread count exceeded. This thread will not be profiled..\n");
  }

  // __sync_bool_compare_and_swap(&g_thread_lock, 1 , 0); // Release lock

}

int SamplingProfiler::getThreadCount() {
  return thread_counter;
}

TLStatistics** SamplingProfiler::getThreadStatistics() {
  return tls_stats;
}

/*
void printM(FuncIDMappings* func_id_mappings) {
 for (std::map<uint16_t,FunctionInfo*>::iterator it=func_id_mappings->begin(); it!=func_id_mappings->end(); ++it)
      std::cout << it->first << " => " << it->second->func_name << '\n'; 
}
*/

void SamplingProfiler::dumpStatistics() {

  fprintf(stderr, "[Sampling Profier] Flushing statistics to disk..\n");

  FILE* fp = fopen("prof.out", "a");

  uint64_t total_count = 0;
  int func_count = ins->getFunctionCount();
  fprintf(stderr, "[finstrumentor] Total function count : %d\n", func_count);
  fprintf(stderr, "[Sampling Profiler] Thread count : %d\n", thread_counter);
  fprintf(fp, "Function,Count,Min_time,Max_time,Avg_time,Deactivation_count,Leaf?\n");
  for(int i=0; i < func_count; i++) {
    statistics[i].count = 0;
    statistics[i].total_time = 0;
    statistics[i].is_leaf = true;
    // statistics[i].deactivation_count = 0;
    statistics[i].limited_count = 0;

    ticks cur_min = ULLONG_MAX, cur_max = 0;
    for(int j=0; j < thread_counter; j++) {
      TLSSamplingProfilerStat* tls_func_stat = (TLSSamplingProfilerStat*) tls_stats[j]->func_stats;
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

      if (!tls_func_stat[i].is_leaf) {
        statistics[i].is_leaf = false;
      }
    }

    total_count += statistics[i].count;
    statistics[i].min_time = cur_min;
    statistics[i].max_time = cur_max;
    
    if (statistics[i].count != 0) {
      fprintf(fp, "%s,%lu,%lu,%lu,%ld,%d,%d\n",  
          ins->getFunctionName(i).c_str(),  statistics[i].count,
          statistics[i].min_time, statistics[i].max_time, 
          statistics[i].total_time / (statistics[i].count - statistics[i].limited_count), 
          statistics[i].deactivation_count, statistics[i].is_leaf); 
    }
  }

  fprintf(stderr, "\n[Sampling Profiler] NUMBER_OF_FUNCTION_CALLS: %lu\n", total_count);

  fclose(fp);

#ifdef OVERHEAD_TIME_SERIES
  fp = fopen("overhead.out", "a");

  for (list<string>:: iterator it = overhead_time_series->begin(); it != overhead_time_series->end(); ++it) {
    fprintf(fp, "%s\n", (*it).c_str());
  }

  fprintf(fp, ">>>>\n");
  fclose(fp);

  fp = fopen("statistics.out", "a");
  fprintf(stderr, "\nNUMBER_OF_FUNCTION_CALLS: %lu\n", total_count);
  fprintf(fp, "SKIPPED_EPOCHS: %lu\n", g_skipped_epochs);
  fprintf(fp, "TOTAL_EPOCHS: %lu\n", g_time_step);
  fclose(fp);

#endif

}

SamplingProfiler::~SamplingProfiler() {
  dumpStatistics();

  TLStatistics** tls_stat = ((SamplingProfiler*)PROFILER_INSTANCE)->getThreadStatistics();
  int thread_count = ((SamplingProfiler*)PROFILER_INSTANCE)->getThreadCount(); 

  for (int i=0; i < thread_count; i++) {
    g_probe_overheads += tls_stat[i]->thread_local_overhead; 
    g_probe_count += tls_stat[i]->thread_local_count;
  }
  Profiler::cleanupInstrumentor();
  delete (SamplingProfilerStat*)g_ubiprof_stats;

  for (int i = 0; i < 64; i++) {
    delete tls_stats[i];
  }

  delete tls_stats;

}
