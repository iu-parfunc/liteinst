
#include <cstdio>
#include "fprofiler.hpp"
#include <stdlib.h>
#include <limits.h>

using namespace std;

/* Globals for this profiler */
uint64_t fb_sample_size;

// All thread local statistics data
static __thread TLStatistics** all_thread_stats;

// Thread local statistics data for current thread
static __thread TLStatistics* current_thread_stats;

// Thread local statistics table
static __thread TLSBackoffProfilerStat* current_thread_func_stats_table;

// Instrumentation Functions
TLStatistics* backoffPrologFunction(uint16_t func_id) {

  static __thread bool allocated;

  if (!allocated) {
    allocated = true;
    uint32_t function_count = INSTRUMENTOR_INSTANCE->getFunctionCount();
    // C++ value initilization.. Similar to calloc
    current_thread_func_stats_table = new TLSBackoffProfilerStat[INSTRUMENTOR_INSTANCE->getFunctionCount()](); 
    current_thread_stats = new TLStatistics;
    current_thread_stats->func_stats = current_thread_func_stats_table;
    current_thread_stats->thread_local_overhead = 0;
    current_thread_stats->thread_local_count = 0;

    for (int i=0; i < function_count; i++) {
      current_thread_func_stats_table[i].is_leaf = true;
      current_thread_func_stats_table[i].stack_depth = 0;
    }

    ((BackoffProfiler*)PROFILER_INSTANCE)->registerThreadStatistics(current_thread_stats);
    all_thread_stats = ((BackoffProfiler*) PROFILER_INSTANCE)->getThreadStatistics();
  }

  BackoffProfilerStat* global_func_stats = &((BackoffProfilerStat*) g_ubiprof_stats)[func_id];
  TLSBackoffProfilerStat* local_func_stats = &current_thread_func_stats_table[func_id];

  uint32_t stack_depth = local_func_stats->stack_depth;

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

  current_thread_stats->thread_local_count++;
  return current_thread_stats;

}

TLStatistics* backoffEpilogFunction(uint16_t func_id) {
  
  ticks epilog_start = getticks();

  BackoffProfilerStat* global_func_stats = &((BackoffProfilerStat*) g_ubiprof_stats)[func_id];
  TLSBackoffProfilerStat* local_func_stats = &current_thread_func_stats_table[func_id];

  int thread_count = ((BackoffProfiler*)PROFILER_INSTANCE)->getThreadCount(); 
  uint32_t stack_depth = local_func_stats->stack_depth;

  if (stack_depth == 0) {
    return current_thread_stats;
  }

  if (stack_depth >= 20 && local_func_stats->ignore_count > 0) {
    local_func_stats->ignore_count--;
    local_func_stats->limited_count++;
    local_func_stats->count++;

    current_thread_stats->thread_local_count++;
    return current_thread_stats;
  }

  InvocationData i_data = local_func_stats->invocation_stack[stack_depth-1];

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

  uint64_t global_count = 0;

  current_thread_stats->thread_local_count++;
  for (int i=0; i < thread_count; i++) {
    // global_count += ((TLSBackoffProfilerStat*) tls_stats[i]-> func_stats)[func_id].count;
    global_count += all_thread_stats[i]->thread_local_count;
  }

  ticks elapsed = epilog_start - i_data.timestamp ;

  if (elapsed < local_func_stats->min_time || local_func_stats->min_time == 0) {
    local_func_stats->min_time = elapsed;
  }

  if (elapsed > local_func_stats->max_time) {
    local_func_stats->max_time = elapsed;
  }

  local_func_stats->total_time += elapsed;

  if ((global_count/2) >= fb_sample_size) { // Since we get prolog + epilog count here
    if (__sync_bool_compare_and_swap(&(global_func_stats->lock), 0 , 1)) {
      PROFILER_INSTANCE->deactivateFunction(&func_id);
      g_deactivation_count++;
      global_func_stats->deactivation_count++; 
      current_thread_stats->deactivated = true;
      __sync_bool_compare_and_swap(&(global_func_stats->lock), 1 , 0);
    }
  }
  
  local_func_stats->stack_depth--;

  return current_thread_stats;

}

// Profiler implementation 
void BackoffProfiler::initialize() {

  Profiler::initInstrumentor(backoffPrologFunction, backoffEpilogFunction);
  statistics = new BackoffProfilerStat[ins->getFunctionCount()](); // C++ value initilization.. Similar to calloc

  // Leaking the reference to the global variable so that 
  // instrumentaion functions can access it without going through object reference
  g_ubiprof_stats = statistics; 

  tls_stats = new TLStatistics*[64](); // C++ value initialization.. Similar to calloc

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

int BackoffProfiler::registerThreadStatistics(TLStatistics* stats) {

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

TLStatistics** BackoffProfiler::getThreadStatistics() {
  return tls_stats;
}

void BackoffProfiler::dumpStatistics() {

  FILE* fp = fopen("prof.out", "a");

  uint64_t total_count = 0;
  int func_count = ins->getFunctionCount();
  fprintf(stderr, "\nTotal function count : %d\n", func_count);
  fprintf(stderr, "Thread count : %d\n", thread_counter);
  fprintf(fp, "Function,Count,Min_time,Max_time,Avg_time,Deactivation_count,Leaf?\n");
  for(int i = 0; i < func_count; i++) {
    statistics[i].count = 0;
    statistics[i].total_time = 0;
    statistics[i].is_leaf = true;
    statistics[i].limited_count = 0;

    ticks cur_min = ULLONG_MAX, cur_max = 0;
    for(int j=0; j < thread_counter; j++) {
      TLSBackoffProfilerStat* tls_func_stat = (TLSBackoffProfilerStat*) tls_stats[j]->func_stats;
      statistics[i].count += tls_func_stat[i].count;
      statistics[i].total_time += tls_func_stat[i].total_time;
      statistics[i].limited_count += tls_func_stat[i].limited_count;

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

  fprintf(stderr, "\n NUMBER_OF_FUNCTION_CALLS: %lu\n", total_count);

  fclose(fp);

}

BackoffProfiler::~BackoffProfiler() {
  dumpStatistics();

  TLStatistics** tls_stat = ((BackoffProfiler*)PROFILER_INSTANCE)->getThreadStatistics();
  int thread_count = ((BackoffProfiler*)PROFILER_INSTANCE)->getThreadCount();

  for (int i=0; i < thread_count; i++) {
    g_probe_overheads += tls_stat[i]->thread_local_overhead;
    g_probe_count += tls_stat[i]->thread_local_count;
  }

  Profiler::cleanupInstrumentor();
  delete (BackoffProfilerStat*)g_ubiprof_stats;

  for (int i = 0; i < 64; i++) {
    delete (TLStatistics*)(tls_stats[i]);
  }

  delete (TLStatistics**) tls_stats;
}
