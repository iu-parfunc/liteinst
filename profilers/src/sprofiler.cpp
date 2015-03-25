
#include "sprofiler.hpp"

#include <cstdio>
#include <pthread.h>
#include <time.h>
#include <sys/time.h>
#include <stdlib.h>
#include <string>
#include <string.h>
#include <list>

using namespace std;

/* Globals for this profiler */
uint64_t sp_initial_sample_size;
uint64_t sp_sample_size;
uint64_t sp_epoch_period;
uint64_t sp_target_overhead;
uint64_t g_total_overhead = 0; // Overhead incurred due to profiling
uint64_t g_total_process_time = 0; // Total process time until last epoch sample
uint64_t g_last_epoch_random = 0; // Random added to last epoch period
uint64_t g_TicksPerNanoSec = 0; // Calibrated ticks per nano second
uint64_t g_call_overhead = 0; // Call overhead calibrated value
uint16_t g_strategy = SLOW_RAMP_UP; // Overhead control strategy to use

#ifdef OVERHEAD_TIME_SERIES
uint64_t g_time_step = 0; // Current time step in epoch time series
uint64_t g_skipped_epochs = 0; // Number skpped epochs

// List to hold the time series data
list<string>* overhead_time_series;
#endif

// All thread local statistics data
static __thread TLStatistics** tl_stats;

// Thread local statistics data for current thread
static __thread TLStatistics* tl_stat;

// Thread local statistics table of current thread 
static __thread TLSSamplingProfilerStat* tl_func_stats;

// Instrumentation Functions
TLStatistics* samplingPrologFunction(uint16_t func_id) {

  static __thread bool allocated;

  if (!allocated) {
    allocated = true;
    // C++ value initilization.. Similar to calloc
    tl_func_stats = new TLSSamplingProfilerStat[INSTRUMENTOR_INSTANCE->getFunctionCount()]();
    tl_stat = new TLStatistics;
    tl_stat->func_stats = tl_func_stats;
    tl_stat->thread_local_overhead = 0;
    tl_stat->thread_local_count = 0;

    ((SamplingProfiler*)PROFILER_INSTANCE)->registerThreadStatistics(tl_stat);
    tl_stats = ((SamplingProfiler*)PROFILER_INSTANCE)->getThreadStatistics();
  }

  tl_func_stats[func_id].start_timestamp = getticks();
  tl_stat->thread_local_count++;
  return tl_stat;

}

TLStatistics* samplingEpilogFunction(uint16_t func_id) {

  ticks epilog_start = getticks();

  SamplingProfilerStat* g_stats = (SamplingProfilerStat*) g_ubiprof_stats;

  int thread_count = ((SamplingProfiler*)PROFILER_INSTANCE)->getThreadCount(); 

  // We do this at epilog itself to get an accurate count than a periodically accumilated count by the probe monitor thread
  uint64_t global_count = 0;
  tl_func_stats[func_id].count++;

  for (int i=0; i < thread_count; i++) {
    global_count += ((TLSSamplingProfilerStat*) tl_stats[i]->func_stats)[func_id].count; 
  }

  uint64_t new_count = global_count - g_stats[func_id].count_at_last_activation; 

  ticks elapsed = epilog_start - tl_func_stats[func_id].start_timestamp ;
  tl_func_stats[func_id].total_time += elapsed;
  if (new_count >= g_stats[func_id].sample_size) {
    if (__sync_bool_compare_and_swap(&(g_stats[func_id].lock), 0 , 1)) {
      PROFILER_INSTANCE->deactivateFunction(&func_id);
      g_stats[func_id].count_at_last_activation = global_count;
      g_stats[func_id].deactivation_count++;
      g_stats[func_id].is_active = false;
      __sync_bool_compare_and_swap(&(g_stats[func_id].lock), 1 , 0);
    }
  }

  tl_stat->thread_local_count++;
  return tl_stat;

}

#ifdef OVERHEAD_TIME_SERIES
void record_overhead_histogram(uint64_t overhead, int64_t sample_size) {

    g_time_step++;

    char buf[50];
    int r = snprintf(buf, 50, "%lu,%lu,%ld\n", g_time_step, overhead, sample_size);
    if (r > 0) {
      string s = buf;
      overhead_time_series->push_back(s);
    }

}
#endif

// Probe monitor
void* samplingProbeMonitor(void* param) {

  SamplingProfilerStat* g_stats = (SamplingProfilerStat*) g_ubiprof_stats;

  while(true) {
    int func_count = INSTRUMENTOR_INSTANCE->getFunctionCount();
    int thread_count = ((SamplingProfiler*)PROFILER_INSTANCE)->getThreadCount(); 

    TLStatistics** tls_stat = ((SamplingProfiler*)PROFILER_INSTANCE)->getThreadStatistics();

    uint64_t thread_overheads = 0;
    uint64_t global_count = 0;
    for (int i=0; i < thread_count; i++) {
      thread_overheads += tls_stat[i]->thread_local_overhead; 
      global_count += tls_stat[i]->thread_local_count;
    }

    uint64_t call_overhead = global_count * g_call_overhead;
    uint64_t cache_perturbation_overhead = global_count * g_cache_miss_overhead_upper_bound;
    // fprintf(stderr, "Call overhead : %lu\n", call_overhead);

    struct timespec ts;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts);
    uint64_t nanoSecs = ts.tv_sec * 1000000000LL + ts.tv_nsec;

    uint64_t probe_thread_overhead = nanoSecs * g_TicksPerNanoSec;  

    uint64_t tmp_total_overhead  = g_total_overhead;
    uint64_t tmp_total_process_time = g_total_process_time;

    // fprintf(stderr, "Initial total overhead : %lu Thread overhead : %lu Probe thread Overhead : %lu\n", 
    //     g_total_overhead, thread_overheads, probe_thread_overhead);
    g_total_overhead = thread_overheads + probe_thread_overhead + call_overhead + 
                       cache_perturbation_overhead + g_init_overhead;

    struct timespec ts1;
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts1);
    nanoSecs = ts1.tv_sec * 1000000000LL + ts1.tv_nsec;

    g_total_process_time = nanoSecs * g_TicksPerNanoSec;
    g_total_process_time -= g_total_overhead;
    // g_total_process_time -= thread_overheads;

    uint64_t overhead_delta = g_total_overhead - tmp_total_overhead;
    uint64_t process_time_delta = g_total_process_time - tmp_total_process_time;

    // uint64_t overhead_of_last_epoch = ((double)overhead_delta / process_time_delta) * 100;
    uint64_t overhead_of_last_epoch = ((double)g_total_overhead / g_total_process_time) * 100;
    // fprintf(stderr, "Per function call overhead (cycles) : %lu\n", g_call_overhead);
    // fprintf(stderr, "Global overhead (cycles) : %lu Global process time : %lu \n", g_total_overhead,
    //     g_total_process_time);
    // fprintf(stderr, "Global overhead delta : %lu Global process delta : %lu \n", overhead_delta,
    //     process_time_delta);
    // fprintf(stderr, "Overhead : %lu\n", overhead_of_last_epoch);

    if (g_strategy == PROPOTIONAL) {
      if (overhead_of_last_epoch != 0 && overhead_of_last_epoch >  sp_target_overhead) {
        uint64_t new_sample_size = ((double)sp_target_overhead / overhead_of_last_epoch) * sp_sample_size; 

        if (new_sample_size > 0) {
          sp_sample_size = new_sample_size;
        } else {
          // Entirely skip probe activation for this sample due to small sample size
          // Too much overhead to control via reducing the sample size
#ifdef OVERHEAD_TIME_SERIES
          g_skipped_epochs++;
          record_overhead_histogram(overhead_of_last_epoch, -1); // -1 signifies no new samples taken in this epoch
#endif
          goto sleep;
        }
      }

      if (overhead_of_last_epoch != 0 && overhead_of_last_epoch <  0.75 * sp_target_overhead) {
        uint64_t new_sample_size = ((double)sp_target_overhead / overhead_of_last_epoch) * sp_sample_size; 

        // Here we don't set the sample size to more than its initial setting to prevent overshooting.
        // TODO: Revisit this
        if (new_sample_size < sp_initial_sample_size) {
          sp_sample_size = new_sample_size;
        } else {
          sp_sample_size = sp_initial_sample_size;
        }
      }
    } else if (g_strategy == SLOW_RAMP_UP) {
      if (overhead_of_last_epoch != 0 && overhead_of_last_epoch >  sp_target_overhead) {
        uint64_t new_sample_size = ((double)sp_target_overhead / overhead_of_last_epoch) * sp_sample_size; 

        if (new_sample_size > 0) {
          sp_sample_size = new_sample_size;
        } else {
          // Entirely skip probe activation for this sample due to small sample size
          // Too much overhead to control via reducing the sample size
#ifdef OVERHEAD_TIME_SERIES
          g_skipped_epochs++;
          record_overhead_histogram(overhead_of_last_epoch, -1); // -1 signifies no new samples taken in this epoch
#endif
          goto sleep;
        }
      }

      double fudge_factor = 1;
      if (overhead_of_last_epoch != 0 && overhead_of_last_epoch <  fudge_factor * sp_target_overhead) {
	double new_target_overhead = overhead_of_last_epoch + (fudge_factor * (double) sp_target_overhead - overhead_of_last_epoch) / 2;
        uint64_t new_sample_size = ((double)new_target_overhead / overhead_of_last_epoch) * sp_sample_size; 

        // Here we don't set the sample size to more than its initial setting to prevent overshooting.
        // TODO: Revisit this
        /*
        if (new_sample_size < sp_initial_sample_size) {
          sp_sample_size = new_sample_size;
        } else {
          sp_sample_size = sp_initial_sample_size;
        }
       */
       if (new_sample_size > sp_sample_size) {
         sp_sample_size = new_sample_size; 
       }
      }
    }
    
#ifdef OVERHEAD_TIME_SERIES
    record_overhead_histogram(overhead_of_last_epoch, sp_sample_size);
#endif

    // fprintf(stderr, "New sample size : %lu\n", sp_sample_size);
      
    for(int i = 0; i < func_count; i++) {
      if (!g_stats[i].is_active) {
        g_stats[i].sample_size = sp_sample_size;
        PROFILER_INSTANCE->activateFunction(&i);
        g_stats[i].is_active = true;
      }
    }

sleep:
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
    statistics[i].is_active = true;
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

  char* adaptive_strategy_str = getenv("ADAPTIVE_STRATEGY");
  if (adaptive_strategy_str != NULL) {
    if (!strcmp(adaptive_strategy_str, "SLOW_RAMP_UP")) {
	g_strategy = SLOW_RAMP_UP;
    } else {
        g_strategy = PROPOTIONAL;
    } 
  } 

  fprintf(stderr, "[Sampling Profiler] **** Parameters : Sample size => %lu Epoch period => %lu Target overhead => %lu \n", 
      sp_sample_size, sp_epoch_period, sp_target_overhead); 

  switch(g_strategy) {
    case SLOW_RAMP_UP:
      fprintf(stderr, "[Sampling Profiler] Adaptive Strategy : SLOW_RAMP_UP\n");
      break;
    default:
      fprintf(stderr, "[Sampling Profiler] Adaptive Strategy : PROPOTIONAL\n");
  }

  spawnMonitor();

}

void SamplingProfiler::spawnMonitor() {

  pthread_create(&g_monitor_thread, NULL, samplingProbeMonitor, (void*)NULL);

}

void SamplingProfiler::registerThreadStatistics(TLStatistics* stats) {

  if (thread_counter+ 1 < 64) {
    tls_stats[thread_counter++] = stats;
  } else {
    fprintf(stderr, "[Sampling Profiler] Max thread count exceeded. This thread will not be profiled..\n");
  }

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
  for(int i=0; i < func_count; i++) {
    statistics[i].count = 0;
    statistics[i].total_time = 0;

    for(int j=0; j < thread_counter; j++) {
      TLSSamplingProfilerStat* tls_func_stat = (TLSSamplingProfilerStat*) tls_stats[j]->func_stats;
      statistics[i].count += tls_func_stat[i].count;
      statistics[i].total_time += tls_func_stat[i].total_time;
    }

    total_count += statistics[i].count;
    
    if (statistics[i].count != 0) {
      fprintf(fp, "Function Name: %s Count %lu Deactivation Count : %d Avg time (cycles) : %lu\n", 
          ins->getFunctionName(i).c_str(),  statistics[i].count, 
          statistics[i].deactivation_count, statistics[i].total_time / statistics[i].count); 
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
