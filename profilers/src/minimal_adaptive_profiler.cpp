
#include <cstdio>
#include <pthread.h>
#include <time.h>
#include <sys/time.h>
#include <stdlib.h>
#include <string>
#include <string.h>
#include <list>
#include <limits.h>

#include "adaptive_profiler.hpp"
#include "overhead_series.hpp"

using namespace std;

// All thread statistics data thread local reference
static __thread TLStatistics** all_thread_stats;

// Thread statistics data for current thread
static __thread TLStatistics* current_thread_stats;

// Function statistics table for current thread 
static __thread TLSAdaptiveProfilerStat* current_thread_func_stats_table;

// Instrumentation Functions
TLStatistics* minimalAdaptivePrologFunction(uint16_t func_id) {

  static __thread bool allocated;

  if (!allocated) {
    allocated = true;
    long function_count = INSTRUMENTOR_INSTANCE->getFunctionCount();
    // C++ value initilization.. Similar to calloc
    current_thread_func_stats_table = new TLSAdaptiveProfilerStat[function_count]();
    current_thread_stats = new TLStatistics;
    current_thread_stats->func_stats = current_thread_func_stats_table;
    current_thread_stats->thread_local_overhead = 0;
    current_thread_stats->thread_local_count = 0;

    ((AdaptiveProfiler*)PROFILER_INSTANCE)->registerThreadStatistics(current_thread_stats);
    all_thread_stats = ((AdaptiveProfiler*)PROFILER_INSTANCE)->getThreadStatistics();
  }

  AdaptiveProfilerStat* global_func_stats = &((AdaptiveProfilerStat*) g_ubiprof_stats)[func_id];
  TLSAdaptiveProfilerStat* local_func_stats = &current_thread_func_stats_table[func_id];

  local_func_stats->invocation_stack[0].func_id = func_id;
  local_func_stats->invocation_stack[0].timestamp = getticks();

  current_thread_stats->thread_local_count++;
  return current_thread_stats;

}

TLStatistics* minimalAdaptiveEpilogFunction(uint16_t func_id) {

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
      PROFILER_INSTANCE->deactivateFunction(&func_id);
      global_func_stats->deactivation_count++; // Store in thread local structure and we sum all TL stuff when flushing results
      global_func_stats->count_at_last_activation = global_count;
      global_func_stats->active = false;
      __sync_bool_compare_and_swap(&(global_func_stats->lock), 1 , 0);
    }
  }


  current_thread_stats->thread_local_count++;
  return current_thread_stats;

}

/*
// Probe monitor
void* minimalAdaptiveProbeMonitor(void* param) {

  if (g_shutting_down_flag == TERMINATE_REQUESTED) {
    g_shutting_down_flag = TERMINATED; // Signals the destructor thread that this thread is done executing
    return NULL;
  }

  AdaptiveProfilerStat* global_stats = (AdaptiveProfilerStat*) g_ubiprof_stats;

  while(true) {

    if (g_shutting_down_flag == TERMINATE_REQUESTED) {
      g_shutting_down_flag = TERMINATED; // Signals the destructor thread that this thread is done executing
      return NULL;
    }

    int func_count = INSTRUMENTOR_INSTANCE->getFunctionCount();
    int thread_count = ((AdaptiveProfiler*)PROFILER_INSTANCE)->getThreadCount(); 

    TLStatistics** tls_stat = ((AdaptiveProfiler*)PROFILER_INSTANCE)->getThreadStatistics();

    uint64_t thread_overheads = 0;
    uint64_t global_count = 0;
    for (int i=0; i < thread_count; i++) {
      thread_overheads += tls_stat[i]->thread_local_overhead; 
      global_count += tls_stat[i]->thread_local_count;
    }

    uint64_t call_overhead = global_count * g_call_overhead;
    uint64_t cache_perturbation_overhead = global_count * g_cache_miss_overhead_upper_bound;

    struct timespec ts;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts);
    uint64_t nanoSecs = ts.tv_sec * 1000000000LL + ts.tv_nsec;

    uint64_t probe_thread_overhead = nanoSecs * g_TicksPerNanoSec;  

    uint64_t tmp_total_overhead  = g_total_overhead;
    uint64_t tmp_total_process_time = g_total_process_time;

    g_total_overhead = thread_overheads + probe_thread_overhead + call_overhead + 
                       cache_perturbation_overhead + g_init_overhead;

    struct timespec ts1;
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts1);
    nanoSecs = ts1.tv_sec * 1000000000LL + ts1.tv_nsec;

    g_total_process_time = nanoSecs * g_TicksPerNanoSec;
    
    if (g_total_process_time > g_total_overhead) {
      g_total_process_time -= g_total_overhead;
    } else {
      fprintf(stderr, "[DEBUG] Boo \n");
    }

    uint64_t overhead_delta = g_total_overhead - tmp_total_overhead;
    uint64_t process_time_delta = g_total_process_time - tmp_total_process_time;

    // fprintf(stderr, "Total overhead : %lu Total process time : %lu Thread overhead : %lu Probe thread Overhead : %lu\n", 
    //        g_total_overhead, g_total_process_time, thread_overheads, probe_thread_overhead); 

    // uint64_t overhead_at_last_epoch = ((double)overhead_delta / process_time_delta) * 100;
    double overhead_at_last_epoch = ((double)g_total_overhead / g_total_process_time) * 100;
    // fprintf(stderr, "Per function call overhead (cycles) : %lu\n", g_call_overhead);
    // fprintf(stderr, "Global overhead (cycles) : %lu Global process time : %lu \n", g_total_overhead,
    //     g_total_process_time);
    // fprintf(stderr, "Global overhead delta : %lu Global process delta : %lu \n", overhead_delta,
    //     process_time_delta);

    // *** Turn this on for runtime overhead logging **
    // fprintf(stderr, "Overhead : %lf\n", overhead_at_last_epoch);

    if (g_strategy == PROPOTIONAL) {
      if (overhead_at_last_epoch != 0 && overhead_at_last_epoch >  sp_target_overhead) {
        uint64_t new_sample_size = ((double)sp_target_overhead / overhead_at_last_epoch) * sp_sample_size; 

        if (new_sample_size > 0) {
          sp_sample_size = new_sample_size;
        } else {
          // Entirely skip probe activation for this sample due to small sample size
          // Too much overhead to control via reducing the sample size
#ifdef OVERHEAD_TIME_SERIES
          g_skipped_epochs++;
          record_overhead_histogram(overhead_at_last_epoch, -1); // -1 signifies no new samples taken in this epoch
#endif
          goto sleep;
        }
      }

      if (overhead_at_last_epoch != 0 && overhead_at_last_epoch <  0.75 * sp_target_overhead) {
        uint64_t new_sample_size = ((double)sp_target_overhead / overhead_at_last_epoch) * sp_sample_size; 

        // Here we don't set the sample size to more than its initial setting to prevent overshooting.
        // TODO: Revisit this
        if (new_sample_size < sp_initial_sample_size) {
          sp_sample_size = new_sample_size;
        } else {
          sp_sample_size = sp_initial_sample_size;
        }
      }

#ifdef OVERHEAD_TIME_SERIES
    record_overhead_histogram(overhead_at_last_epoch, sp_epoch_period);
#endif

      for(int i = 0; i < func_count; i++) {
        if (!global_stats[i].active) {
          global_stats[i].sample_size = sp_sample_size;
          PROFILER_INSTANCE->activateFunction(&i);
          global_stats[i].active = true;
        } else { // TODO: Reset all function sample sizes
          __sync_bool_compare_and_swap(&global_stats[i].sample_size, global_stats[i].sample_size, sp_sample_size); // Atomically set the value
        }
      }

    } else if (g_strategy == SLOW_RAMP_UP) {
      if (overhead_at_last_epoch != 0 && overhead_at_last_epoch >  sp_target_overhead) {
        uint64_t new_sample_size = ((double)sp_target_overhead / overhead_at_last_epoch) * sp_sample_size; 

        if (new_sample_size > 0) {
          sp_sample_size = new_sample_size;
        } else {
          // Entirely skip probe activation for this sample due to small sample size
          // Too much overhead to control via reducing the sample size
#ifdef OVERHEAD_TIME_SERIES
          g_skipped_epochs++;
          record_overhead_histogram(overhead_at_last_epoch, -1); // -1 signifies no new samples taken in this epoch
#endif
          goto sleep;
        }
      }

      double fudge_factor = 1;
      if (overhead_at_last_epoch != 0 && overhead_at_last_epoch <  fudge_factor * sp_target_overhead) {
	      double new_target_overhead = overhead_at_last_epoch + (fudge_factor * (double) sp_target_overhead - overhead_at_last_epoch) / 2;
        uint64_t new_sample_size = ((double)new_target_overhead / overhead_at_last_epoch) * sp_sample_size; 

        // Here we don't set the sample size to more than its initial setting to prevent overshooting.
        // TODO: Revisit this
 
       if (new_sample_size > sp_sample_size) {
         sp_sample_size = new_sample_size; 
       }
      }

#ifdef OVERHEAD_TIME_SERIES
    record_overhead_histogram(overhead_at_last_epoch, sp_epoch_period);
#endif

      for(int i = 0; i < func_count; i++) {
        if (!global_stats[i].active) {
          global_stats[i].sample_size = sp_sample_size;
          PROFILER_INSTANCE->activateFunction(&i);
          global_stats[i].active = true;
        } else { // TODO: Reset all function sample sizes
          __sync_bool_compare_and_swap(&global_stats[i].sample_size, global_stats[i].sample_size, sp_sample_size); // Atomically set the value
        }
      }

    } else if (g_strategy == EPOCH_CONTROL) {
      if (overhead_at_last_epoch != 0 && overhead_at_last_epoch >  sp_target_overhead) {
     //   fprintf(stderr, "[ubiprof] Overhead : %.2lf Target overhead : %.2lf Epoch period : %.2lf\n", 
     //       overhead_at_last_epoch, sp_target_overhead, sp_epoch_period);
        double new_epoch_period = ((double) overhead_at_last_epoch /sp_target_overhead) * sp_epoch_period; 
     //   fprintf(stderr, "[ubiprof] New epoch period : %.2lf\n", new_epoch_period);

        if (new_epoch_period > 0) {
          sp_epoch_period = new_epoch_period;
        } else {
          // Entirely skip probe activation for this sample due to small sample size
          // Too much overhead to control via reducing the sample size
#ifdef OVERHEAD_TIME_SERIES
          g_skipped_epochs++;
          record_overhead_histogram(overhead_at_last_epoch, -1); // -1 signifies no new samples taken in this epoch
#endif
          goto sleep;
        }
      }

      double fudge_factor = 1;
      if (overhead_at_last_epoch != 0 && overhead_at_last_epoch <  fudge_factor * sp_target_overhead) {
	      double new_target_overhead = overhead_at_last_epoch + (fudge_factor * (double) sp_target_overhead - overhead_at_last_epoch) / 2;
      //  fprintf(stderr, "[ubiprof] Overhead : %.2lf New Target Overhead : %.2lf Epoch period : %.2lf\n", 
      //      overhead_at_last_epoch, new_target_overhead, sp_epoch_period);
        double new_epoch_period =  ((double) overhead_at_last_epoch /new_target_overhead) * sp_epoch_period; 
      //  fprintf(stderr, "[ubiprof] New epoch period : %.2lf\n", new_epoch_period);

       if (new_epoch_period < sp_epoch_period) {
         sp_epoch_period = new_epoch_period; 
       }
      }

      // fprintf(stderr, "[ubiprof] Epoch Size : %lu\n", sp_epoch_period);

      // Reactivate all the deactivated methods
      for(int i = 0; i < func_count; i++) {
        if (!global_stats[i].active) {
          global_stats[i].sample_size = sp_sample_size; // Sample size doesn't change
          PROFILER_INSTANCE->activateFunction(&i);
          global_stats[i].active = true;
        }      
      }

#ifdef OVERHEAD_TIME_SERIES
    record_overhead_histogram(overhead_at_last_epoch, sp_epoch_period);
#endif

    }

    // fprintf(stderr, "New sample size : %lu\n", sp_sample_size);
      

sleep:
    if (sp_epoch_period != 0) {
      uint64_t nanos = sp_epoch_period * 1000000; 
      uint64_t secs = nanos / 1000000000;
      uint64_t nsecs = nanos % 1000000000;
      ts.tv_sec = secs;
      ts.tv_nsec = nsecs;
      nanosleep(&ts, NULL);
    }

  }
}
*/

// Profiler implementation 
void MinimalAdaptiveProfiler::initialize() {

  Profiler::initInstrumentor(minimalAdaptivePrologFunction, minimalAdaptiveEpilogFunction);

#if OVERHEAD_TIME_SERIES
  overhead_time_series = new list<string>();
#endif

  long func_count = ins->getFunctionCount();
  fprintf(stderr, "[DEBUG] Adaptive Profiler function count : %ld\n", func_count);
  // func_count++; // Hack to add calibrate_cache_effect
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
    } else {
      g_strategy = EPOCH_CONTROL;
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

/*
void MinimalAdaptiveProfiler::spawnMonitor() {

  pthread_create(&g_monitor_thread, NULL, minimalAdaptiveProbeMonitor, (void*)NULL);

}
*/

/*
void AdaptiveProfiler::registerThreadStatistics(TLStatistics* stats) {
  // while(__sync_bool_compare_and_swap(&g_thread_lock, 0 , 1)); // Acquire lock

  if (thread_counter+ 1 < 64) {
    tls_stats[thread_counter++] = stats;
  } else {
    fprintf(stderr, "[Adaptive Profiler] Max thread count exceeded. This thread will not be profiled..\n");
  }

  // __sync_bool_compare_and_swap(&g_thread_lock, 1 , 0); // Release lock

}

int AdaptiveProfiler::getThreadCount() {
  return thread_counter;
}

TLStatistics** AdaptiveProfiler::getThreadStatistics() {
  return tls_stats;
}
*/

/*
void printM(FuncIDMappings* func_id_mappings) {
 for (std::map<uint16_t,FunctionInfo*>::iterator it=func_id_mappings->begin(); it!=func_id_mappings->end(); ++it)
      std::cout << it->first << " => " << it->second->func_name << '\n'; 
}
*/

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
      fprintf(fp, "%s,%lu,%lu,%lu,%ld,%d\n",  
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
