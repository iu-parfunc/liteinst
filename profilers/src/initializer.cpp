
#include "profiler.hpp"
#include "fprofiler.hpp"
#include <papi.h> 

#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "sprofiler.hpp"
#include "globals.hpp"

#define FIXED_BACKOFF 0
#define SAMPLING 1
#define EMPTY 2
#define ADAPTIVE 3

/* Global to hold the profiler type */
int g_profiler_type = 0;

volatile uint64_t g_deactivation_count;
uint64_t g_cache_miss_overhead_upper_bound = 0;
uint64_t g_init_overhead;

bool g_ubiprof_initialized = false;

struct timespec g_begints;
struct timespec g_endts;
struct timespec g_diff;

extern void print_probe_info();

#ifdef __cplusplus
extern "C"
{
  void __cyg_profile_func_enter(void *this_fn, void *call_site)
    __attribute__((no_instrument_function));
  void __cyg_profile_func_exit(void *this_fn, void *call_site)
    __attribute__((no_instrument_function));
}
#endif

__attribute__((no_instrument_function))
struct timespec *timeSpecDiff(struct timespec *ts1, struct timespec *ts2) {
  static struct timespec ts;
  g_diff.tv_sec = ts1->tv_sec - ts2->tv_sec;
  g_diff.tv_nsec = ts1->tv_nsec - ts2->tv_nsec;
  
  if (g_diff.tv_nsec < 0) {
    g_diff.tv_sec--;
    g_diff.tv_nsec += 1000000000LL;
  }

  return &g_diff;
}

double getSecondsFromTicks(uint64_t ticks) {
  return (double)ticks / (g_TicksPerNanoSec * 1000000000); 
}

double getSecondsFromTS(timespec* ts) {
  return ts->tv_sec + (double)ts->tv_nsec/ 1000000000; 
}

void __attribute__ ((noinline)) emptyFunc(uint64_t* a, uint64_t* b) {

}

uint64_t get_average_time(ticks* elapsed, int size) {

  int BIN_SIZE = 100;
  int HIST_SIZE = 21; // 21 bins of 100 cycles width. 2000+ to a single bin
  int histogram[HIST_SIZE];
  for (int i=0; i<HIST_SIZE; i++) {
    histogram[i] = 0;
  }

  for (int i=0; i<size; i++) {
    if (elapsed[i] > 2100) {
      histogram[HIST_SIZE-1]++;
    } else {
      histogram[elapsed[i]/BIN_SIZE]++;
    }
  }

  int max_bin = 0;
  for (int i=0; i<HIST_SIZE; i++) {
    if (histogram[i] > histogram[max_bin]) {
        max_bin = i;
    }
  }

  uint64_t sum = 0;
  for (int i=0; i<size; i++) {
    if (elapsed[i] > 2100 && max_bin == HIST_SIZE -1) {
      sum += elapsed[i];
    } else if (elapsed[i]/BIN_SIZE == max_bin) {
      sum += elapsed[i];
    }
  }

  return sum/histogram[max_bin];

}

__attribute__((no_instrument_function))
void calibrate_cache_effects() {
  size_t cache_size = sysconf(_SC_LEVEL3_CACHE_SIZE);
  size_t cache_line_size = sysconf(_SC_LEVEL3_CACHE_LINESIZE); 
  long cache_lines = 0;
  if (cache_line_size != 0) {
    cache_lines = cache_size / cache_line_size; 
  } else {
    cache_lines = cache_size / 8; 
  }

  size_t num_cpus = sysconf(_SC_NPROCESSORS_ONLN);

  fprintf(stderr, "[Ubiprof] L3 Cache size (MB) : %lu\n", (uint64_t)((double) cache_size) / (1024 * 1024)); 
  fprintf(stderr, "[Ubiprof] Number of cache lines : %lu\n", cache_lines);
  fprintf(stderr, "[Ubiprof] Number of processors : %lu\n", num_cpus);

  double FUDGE = 1;
  size_t alloc_size = sysconf(_SC_LEVEL3_CACHE_SIZE) * (num_cpus + 1) * 2;

  fprintf(stderr, "[DEBUG] Cache mult : %lu\n", (num_cpus + 1) * 2);

  long num_cache_lines = alloc_size / cache_line_size;
  char *a, *b;
  a = (char*) malloc(alloc_size); // Allocates two times the caches just to be sure
  if (!a) {
    fprintf(stderr, "[Ubiprof] ERROR : Faliure allocating memory in cache effect calibration.\n");
    return;
  }

  // We need to add calibration method to instrumentor meta data since
  // this method has not been actually instrumented using finstrument.
  // Also we need to add this information explicitly for shared library
  // functions since we don't explicitly handle finstrumented shared 
  // libraries at the moment.
  INSTRUMENTOR_INSTANCE->addFunction((uint64_t)&calibrate_cache_effects, "calibrate_cache_effects");;

  // Warm the cache. Incidently we don't capture initial setup and patching
  // overhead due to this. But it should be ok since it is one time and
  // would not effect that much since it would get amortized in the long 
  // run even if accounted for.
  for (int i=0; i<19; i++) {
    // Invoke instrumentation explicitly. We use the second param to signal that this is a special invocation 
    __cyg_profile_func_enter((void*)&calibrate_cache_effects, (void*)-1); 
    __cyg_profile_func_exit((void*)&calibrate_cache_effects, (void*)-1); 
  }

  int rounds = 100;
  // Get warm cache results
  ticks elapsed[rounds];
  for (int i=0; i<rounds; i++) {
    // ticks start = getstart();
    ticks start = getticks();
    __cyg_profile_func_enter((void*)&calibrate_cache_effects, (void*)-1); 
    __cyg_profile_func_exit((void*)&calibrate_cache_effects, (void*)-1); 
    // ticks end = getend();
    ticks end = getticks();
    elapsed[i] = (end - start);
  }


  uint64_t warm_cache_avg_time = get_average_time(elapsed, rounds);
    
  // Get cold cache results
  for (int i=0; i<rounds; i++) {
    // Trash the cache
    uint64_t sum = 0;
    for (int j=0; j<num_cache_lines; j+=cache_line_size) {
      sum += a[j];
    }

    // fprintf(stderr, "[DEBUG] Hey we are here...\n");
    // ticks start = getstart();
    ticks start = getticks();
    __cyg_profile_func_enter((void*)&calibrate_cache_effects, (void*)-1); 
    __cyg_profile_func_exit((void*)&calibrate_cache_effects, (void*)-1); 
    // ticks end = getend();
    ticks end = getticks();

    elapsed[i] = (end - start);

  }

  uint64_t cold_cache_avg_time = get_average_time(elapsed, rounds);
  int64_t difference = cold_cache_avg_time - warm_cache_avg_time;

  if (difference < 0) {
    fprintf(stderr,"[Ubiprof] ERROR : Cache calibration returned invalid result." 
            " No cache perturbation compensation will be carried out.\n");
    g_cache_miss_overhead_upper_bound = 0;
    return;
  }

  g_cache_miss_overhead_upper_bound = difference; 

  fprintf(stderr, "[Ubiprof] Cold cache instrumentation cost : %lu "
          "Warm cache instrumentation cost : %lu\n", cold_cache_avg_time, warm_cache_avg_time);
  fprintf(stderr, "[Ubiprof] Cache perturbation overhead : %lu\n", 
          g_cache_miss_overhead_upper_bound);

  free(a);

}

/*
__attribute__((no_instrument_function))
void calibrate_cache_effects_1() {

  // Allocate 2 L3 cache sized memory chunks
  // for i..3
  //   Access one of it element by element
  //   PAPI_start();
  //   _cyg_enter(&calibrate_cache_effects, 0)
  //   _cyge_exit(&calibrate_cahce_effects, 0)
  //   PAPI_end();
  // get the median of L3 misses values
  // Acesss on chunk element by element
  // start = getticks()
  // Acess second chunk element by element
  // end = getticks()
  // cache_perturbation_overhead = L3_misses * ((end - start) / L3 cache wordsize)
  // delete memory chunks
  
  size_t cache_size = sysconf(_SC_LEVEL3_CACHE_SIZE);
  size_t cache_line_size = sysconf(_SC_LEVEL3_CACHE_LINESIZE); 
  long cache_lines = 0;
  if (cache_line_size != 0) {
    cache_lines = cache_size / cache_line_size; 
  } else {
    cache_lines = cache_size / 8; 
  }

  fprintf(stderr, "[Ubiprof] L3 Cache size (MB) : %lu\n", (uint64_t)((double) cache_size) / (1024 * 1024)); 
  fprintf(stderr, "[Ubiprof] Number of cache lines : %lu..\n", cache_lines);

  double FUDGE = 1;
  char *a, *b;
  a = (char*) malloc(sysconf(_SC_LEVEL3_CACHE_SIZE));
  if (!a) {
    fprintf(stderr, "[Ubiprof] ERROR : Faliure allocating memory in cache effect calibration.\n");
    return;
  }
  b = (char*) malloc(sysconf(_SC_LEVEL3_CACHE_SIZE));
  
  // Initialize PAPI
  int retval;
  int eventSet = PAPI_NULL;

  int rounds=100;
  long long values[rounds];
  for (int i=0; i<rounds; i++) {  
    values[i] = 0;
  }

  PAPI_event_info_t evinfo;
  PAPI_mh_level_t *L;

  const int eventlist[] = {PAPI_L3_TCM}; // L3 cache misses
  if ((retval = PAPI_library_init(PAPI_VER_CURRENT)) != PAPI_VER_CURRENT) {
    fprintf(stderr, "[Ubiprof] ERROR 1 calibrating for cache effects..\n");
    return;
  }

  if ((retval = PAPI_create_eventset(&eventSet)) != PAPI_OK) {
    fprintf(stderr, "[Ubiprof] ERROR 2 calibrating for cache effects..\n");
    return;
  }

  if (PAPI_add_event(eventSet, eventlist[0]) != PAPI_OK) {
    fprintf(stderr, "[Ubiprof] ERROR 3 calibrating for cache effects..\n");
    return;
  }

  if ((retval = PAPI_start(eventSet)) != PAPI_OK) {
    fprintf(stderr, "[Ubiprof] ERROR 4 calibrating for cache effects..\n");
    return;
  }

  for (int i=0; i<rounds; i++) {
    // Trash the cache
    uint64_t sum = 0;
    for (int j=0; j<cache_lines; j+=cache_line_size) {
      sum += a[j];
    }

    if ((retval = PAPI_reset(eventSet)) != PAPI_OK) { 
      fprintf(stderr, "[Ubiprof] ERROR 6 calibrating for cache effects..\n");
      return;
    }

    ticks start = getstart();
    __cyg_profile_func_enter((void*)&calibrate_cache_effects, (void*)-1); 
    __cyg_profile_func_exit((void*)&calibrate_cache_effects, (void*)-1); 
    ticks end = getend();

    elapsed[i] = (end - start);

    if ((retval = PAPI_read(eventSet, &values[i])) != PAPI_OK) {
      fprintf(stderr, "[Ubiprof] ERROR 5 calibrating for cache effects..\n");
      return;
    }

    if ((retval = PAPI_reset(eventSet)) != PAPI_OK) { 
      fprintf(stderr, "[Ubiprof] ERROR 100 calibrating for cache effects..\n");
      return;
    } 
  }

  if ((retval = PAPI_stop(eventSet, NULL)) != PAPI_OK) {
    fprintf(stderr, "[Ubiprof] ERROR 7 calibrating for cache effects..\n");
  }

  if ((retval = PAPI_remove_event(eventSet, eventlist[0])) != PAPI_OK) {
    fprintf(stderr, "[Ubiprof] ERROR 8 calibrating for cache effects..\n");
  }

  if ((retval = PAPI_destroy_eventset(&eventSet)) != PAPI_OK) {
    fprintf(stderr, "[Ubiprof] ERROR 9 calibrating for cache effects..\n");
  }

  fprintf(stderr, "[Ubiprof] cache_misses[0] : %lld cache_misses[1] : %lld cache_misses[2] : %lld\n",
          values[0], values[1], values[2]);

  uint64_t cache_misses = 0; 
  for (int j=0; j<rounds; j++) {
    fprintf(stderr, "[DEBUG-MISSES] Cache misses %d : %lu\n", j, values[j]);
    if (cache_misses < values[j]) {
      cache_misses = values[j];
    }
  }

  srand(time(NULL));

  // Find approximately how much time it takes to load a cache line from the memory
  ticks fetch_overhead = ULLONG_MAX;
  for (int i=0; i<rounds; i++) {
    // Trash the cache
    uint64_t sum = 0;
    for (int j=0; j<cache_lines; j+=cache_line_size) {
      sum += a[j];
    }

    int index = (rand() % cache_lines) * cache_line_size; // Load a randomly choosen cache line
    // b[index] = 2;
    ticks start = getticks();
    sum += b[index]; 
    ticks end = getticks();
    ticks current = end - start;

    fprintf(stderr, "[DEBUG] Fetch overhead %d : %lu\n", i, current);
    if (current < fetch_overhead) {
      fetch_overhead = current;
    }
  }

  free(a);
  free(b);

  // fetch_overhead = 12000;

  fprintf(stderr, "[Ubiprof] cache misses %lld fetch overhead %lld\n", cache_misses, fetch_overhead);
  g_cache_miss_overhead_upper_bound = cache_misses * fetch_overhead;
  // g_cache_miss_overhead_upper_bound = 12000;
  fprintf(stderr, "[Ubiprof] Cache perturbation overhead of instrumentation : %lld\n", 
      g_cache_miss_overhead_upper_bound);

}
*/

__attribute__((no_instrument_function))
void calibrateTicks() {
  struct timespec begints, endts, diff;
  uint64_t begin = 0, end = 0;
  clock_gettime(CLOCK_MONOTONIC, &begints);
  begin = getticks();
  uint64_t i, result = 0;
  for (i=0; i < 10000; i++) {
    // result += 1;
    emptyFunc(&i, &result); 
  }
  end = getticks();
  clock_gettime(CLOCK_MONOTONIC, &endts);
  struct timespec *tmpts = timeSpecDiff(&endts, &begints);
  // uint64_t millisecElapsed = tmpts->tv_sec * 1000L + tmpts->tv_nsec / 1000000L;
  // g_ticksPerMilliSec = (double) (end - begin) / (double) millisecElapsed;
  uint64_t nanoSecElapsed = tmpts->tv_sec * 1000000000LL + tmpts->tv_nsec;
  g_TicksPerNanoSec = (double) (end - begin) / (double) nanoSecElapsed;
  g_call_overhead = (end - begin) / 10000;

  // fprintf(stderr, "Call Overhead : %n", g_call_overhead);
}

void getFinalOverhead() {

  uint64_t call_overhead = g_probe_count * g_call_overhead;
  uint64_t cache_perturbation_overhead = g_probe_count * g_cache_miss_overhead_upper_bound;

  double init_overhead = getSecondsFromTicks(g_init_overhead);
  double probe_overhead = getSecondsFromTicks(g_probe_overheads);
  double jump_overhead = getSecondsFromTicks(call_overhead);
  double cache_overhead = getSecondsFromTicks(cache_perturbation_overhead);

  clockid_t cid;
  struct timespec p_ts, main_ts, monitor_ts;

  int c = pthread_getcpuclockid(pthread_self(), &cid);
  if (c != 0) {
    fprintf(stderr, "Error obtaining main thread cpu time ..\n");
  }
  if (clock_gettime(cid, &main_ts) == -1) {
    fprintf(stderr, "Error obtaining main thread cpu time ..\n");
  }

  if (g_monitor_thread != NULL) {
    c = pthread_getcpuclockid(g_monitor_thread, &cid);
    if (c != 0) {
      fprintf(stderr, "Error obtaining monitor thread cpu time ..\n");
    }
    if (clock_gettime(cid, &monitor_ts) == -1) {
      fprintf(stderr, "Error obtaining monitor thread cpu time ..\n");
    }
  }

  if (clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &p_ts) == -1) {
    fprintf(stderr, "Error obtaining process cpu time ..\n");
  }

  double probe_thread_cpu_time = 0;
  if (g_monitor_thread != NULL) {
    probe_thread_cpu_time =  getSecondsFromTS(&monitor_ts);
  }

  double main_thread_cpu_time = getSecondsFromTS(&main_ts);
  double process_cpu_time = getSecondsFromTS(&p_ts);

  double process_overhead_delta = process_cpu_time - main_thread_cpu_time - probe_thread_cpu_time;  
  double calculated_overheads = probe_overhead + jump_overhead + init_overhead + cache_overhead;

  double overhead_at_final_epoch = ((double)g_total_overhead / g_total_process_time) * 100;

#ifdef OVERHEAD_TIME_SERIES
  FILE* fp = fopen("statistics.out", "a");
  fprintf(fp, "DEACTIVATIONS : %lu\n", g_deactivation_count);
  fprintf(fp, "APPLICATION_CPU_TIME(s): %lf\n", main_thread_cpu_time);
  fprintf(fp, "MONITOR_THREAD_CPU_TIME(s): %lf\n", probe_thread_cpu_time);
  fprintf(fp, "PROCESS_CPU_TIME(s): %lf\n", process_cpu_time);
  fprintf(fp, "INIT_OVERHEAD: %lf\n", init_overhead);
  fprintf(fp, "CACHE_PERTURBATION_OVERHEAD: %lf\n", cache_overhead);
  fprintf(fp, "PROBE_OVERHEAD: %lf\n", probe_overhead);
  fprintf(fp, "JUMP_OVERHEAD: %lf\n", jump_overhead);
  fprintf(fp, "CUMULATIVE_OVERHEAD: %lf\n", calculated_overheads);
  fprintf(fp, "REAL_EXEC_TIME: %lf\n\n", main_thread_cpu_time - calculated_overheads);
  fprintf(fp, "TARGET_OVERHEAD: %.2lf\n", (double) sp_target_overhead);
  fprintf(fp, "RUNTIME_OVERHEAD: %.2lf\n", overhead_at_final_epoch);
  fclose(fp);
#endif

  fprintf(stderr, "\n\n\n\n");
  fprintf(stderr, "\n[ubiprof] DEACTIVATIONS : %lu\n", g_deactivation_count);
  fprintf(stderr, "\n");
  fprintf(stderr, "[ubiprof] APPLICATION_CPU_TIME(s): %lf\n", main_thread_cpu_time);
  fprintf(stderr, "[ubiprof] MONITOR_THREAD_CPU_TIME(s): %lf\n", probe_thread_cpu_time);
  fprintf(stderr, "[ubiprof] PROCESS_CPU_TIME(s): %lf\n", process_cpu_time);
  // fprintf(stderr, "[ubiprof] PROCESS_OVERHEAD_DELTA: %lf\n", process_overhead_delta);

  fprintf(stderr, "[ubiprof] INIT_OVERHEAD(S): %lf\n", init_overhead);
  fprintf(stderr, "[ubiprof] CACHE_PERTURBATION_OVERHEAD(s): %lf\n", cache_overhead);
  fprintf(stderr, "[ubiprof] PROBE_OVERHEAD(s): %lf\n", probe_overhead);
  fprintf(stderr, "[ubiprof] JUMP_OVERHEAD(s): %lf\n", jump_overhead);
  fprintf(stderr, "[ubiprof] CUMULATIVE_OVERHEAD(s): %lf\n", calculated_overheads);
  fprintf(stderr, "[ubiprof] REAL_EXEC_TIME(s): %lf\n", main_thread_cpu_time - calculated_overheads);
  fprintf(stderr, "[ubiprof] TARGET_OVERHEAD: %.2lf\n", (double) sp_target_overhead);
  fprintf(stderr, "[ubiprof] RUNTIME_OVERHEAD: %.2lf\n", overhead_at_final_epoch);

  // fprintf(stderr, "[ubiprof] EXEC_TIME: %lf\n", main_thread_cpu_time - probe_overhead);

}

#ifndef NO_INIT
__attribute__((constructor, no_instrument_function))
  void initProfiler() {

    // ticks start = getticks();

    calibrateTicks();

    struct timespec begin_ts;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &begin_ts);
    uint64_t nanoSecs = begin_ts.tv_sec * 1000000000LL + begin_ts.tv_nsec;

    uint64_t start = nanoSecs * g_TicksPerNanoSec;  

    g_deactivation_count = 0;

    clock_gettime(CLOCK_MONOTONIC, &g_begints);


    char* profiler_type_str = getenv("PROFILER_TYPE");
    if (profiler_type_str != NULL) {
      if (!strcmp(profiler_type_str, "FIXED_BACKOFF")) {
        fprintf(stderr, "[Ubiprof] Intializing the Backoff Profiler..\n");
        g_profiler_type = FIXED_BACKOFF;
      } else if (!strcmp(profiler_type_str, "SAMPLING")) {
        fprintf(stderr, "[Ubiprof] Intializing the Sampling Profiler..\n");
        g_profiler_type = SAMPLING;
      } else if (!strcmp(profiler_type_str, "EMPTY")) {
        fprintf(stderr, "[Ubiprof] Intializing the Empty Profiler..\n");
        g_profiler_type = EMPTY;
      } else if (!strcmp(profiler_type_str, "ADAPTIVE")) {
        fprintf(stderr, "[Ubiprof] Intializing the Sampling Profiler..\n");
        g_profiler_type = ADAPTIVE;
      } else if (!strcmp(profiler_type_str, "MINIMAL_ADAPTIVE")) {
        fprintf(stderr, "[Ubiprof] Intializing the Minimal Adaptive Profiler..\n");
        g_profiler_type = MINIMAL_ADAPTIVE;
      } else if (!strcmp(profiler_type_str, "MINIMAL_BACKOFF")) {
        fprintf(stderr, "[Ubiprof] Intializing the Minimal Backoff Profiler..\n");
        g_profiler_type = MINIMAL_BACKOFF;
      } else if (!strcmp(profiler_type_str, "MINIMAL_SAMPLING")) {
        fprintf(stderr, "[Ubiprof] Intializing the Minimal Sampling Profiler..\n");
        g_profiler_type = MINIMAL_SAMPLING;
      } else {
        fprintf(stderr, "[Ubiprof] Intializing the Minimal Adaptive Profiler..\n");
        g_profiler_type = MINIMAL_ADAPTIVE;
      }
    } else {
      fprintf(stderr, "[Ubiprof] Intializing the Minimal Adaptive Profiler..\n");
      g_profiler_type = MINIMAL_ADAPTIVE;
    }

    // Init the suitable profiler depending on enviornment variable
    Profiler::getInstance(g_profiler_type);

    // Calibrate for cache effects
    calibrate_cache_effects();

    fprintf(stderr, "[Ubiprof] Done intializing the profiler..\n\n");
    fprintf(stderr, "[Ubiprof] Calibrated parameter values per probe\n Indirect Jump Cost : %lu  Cache miss cost : %lu\n",
                        2*g_call_overhead, g_cache_miss_overhead_upper_bound);

    // ticks end = getticks();

    g_ubiprof_initialized = true;

    struct timespec end_ts;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &end_ts);
    nanoSecs = end_ts.tv_sec * 1000000000LL + end_ts.tv_nsec;

    uint64_t end = nanoSecs * g_TicksPerNanoSec;  

    g_init_overhead = end - start;

  }
#endif

#ifndef NO_INIT
__attribute__((destructor))
  void destroyProfiler() {


    // Tell probe monitor thread to shurdown 
    if (g_profiler_type == ADAPTIVE) {
      g_shutting_down_flag = TERMINATE_REQUESTED;
      int retries = 0;
      while (g_shutting_down_flag != TERMINATED && retries++ < 2) {
        struct timespec ts;
        uint64_t nanos = 10 * 1000000;  // Sleep 10 milli seconds
        uint64_t secs = nanos / 1000000000;
        uint64_t nsecs = nanos % 1000000000;
        ts.tv_sec = secs;
        ts.tv_nsec = nsecs;
        nanosleep(&ts, NULL);
	// sleep(10);
        // fprintf(stderr, "Trying to finish..\n");
        // sleep(sp_epoch_period); // Sleep for a while until probe monitor thread terminates
        // break;
      }
    }

    clock_gettime(CLOCK_MONOTONIC, &g_endts);
    timeSpecDiff(&g_endts, &g_begints);

    fprintf(stderr, "\n[Ubiprof] Destroying the profiler..\n");
    delete Profiler::getInstance(g_profiler_type);

    getFinalOverhead();

    fprintf(stderr, "\n[ubiprof] UBIPROF_ELAPSED_TIME : %lf\n", getSecondsFromTS(&g_diff));

  } 
#endif
