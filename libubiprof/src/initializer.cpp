
#include "backoff_profiler.hpp"

#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "sampling_profiler.hpp"

/* Global to hold the profiler type */
ProfilerType g_profiler_type = ProfilerType::MINIMAL_SAMPLING;

uint64_t g_cache_miss_overhead_upper_bound = 0;
uint64_t g_init_overhead;

struct timespec g_begints;
struct timespec g_endts;
struct timespec g_diff;

extern void print_probe_info();



// BJS: This breakes the separation of profilers and instrumentors.
//      as it relies on the cyg functions from finstrumentor. 
//      I am guessing that calibrate_cache_effects is really 
//      finstrumentor specific (however a similar function may 
//      need to exist for zca). 
#ifdef __cplusplus
extern "C"
{
  void __cyg_profile_func_enter(void *this_fn, void *call_site)
    __attribute__((no_instrument_function));
  void __cyg_profile_func_exit(void *this_fn, void *call_site)
    __attribute__((no_instrument_function));
  void fake_cyg_profile_func_enter(void *this_fn, void *call_site)
    __attribute__((no_instrument_function));
  void fake_cyg_profile_func_exit(void *this_fn, void *call_site)
    __attribute__((no_instrument_function));
}
#endif


/*

  11/11/15 - BC : Temporarily commented out. Not required for Xmod stuff.

// BJS: I am very confused about why no_instrument_function would 
// ever be needed inside this area of the code. 

__attribute__((no_instrument_function))
struct timespec *timeSpecDiff(struct timespec *ts1, struct timespec *ts2); 

struct timespec *timeSpecDiff(struct timespec *ts1, struct timespec *ts2) {
  //static struct timespec ts;
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

__attribute__((no_instrument_function)) void calibrateTicks(); 

void calibrateTicks() {
  struct timespec begints, endts;//, diff;
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
  // Monitor thread is finished already, so there are no races on these globals:
  uint64_t call_overhead = g_probe_count * g_call_overhead;
  uint64_t cache_perturbation_overhead = g_probe_count * g_cache_miss_overhead_upper_bound;

  double init_overhead  = getSecondsFromTicks(g_init_overhead);
  double probe_overhead = getSecondsFromTicks(g_probe_overheads);
  double jump_overhead  = getSecondsFromTicks(call_overhead);
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

  if (g_monitor_thread != 0) {
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
  if (g_monitor_thread != 0) {
    probe_thread_cpu_time =  getSecondsFromTS(&monitor_ts);
  }

  double main_thread_cpu_time = getSecondsFromTS(&main_ts);
  double process_cpu_time = getSecondsFromTS(&p_ts);

  //  double process_overhead_delta = process_cpu_time - main_thread_cpu_time - probe_thread_cpu_time;  
  double calculated_overheads = probe_overhead + jump_overhead + init_overhead + cache_overhead;

  double overhead_at_final_epoch = ((double)g_total_overhead / g_total_process_time) * 100;

#ifdef OVERHEAD_TIME_SERIES
  FILE* fp = fopen("statistics.out", "a");
  fprintf(fp, "DEACTIVATIONS : %lu\n", g_deactivation_count);
  // TODO: Rename to MAIN_THREAD_CPU_TIME:
  fprintf(fp, "APPLICATION_CPU_TIME(s): %lf\n", main_thread_cpu_time);
  fprintf(fp, "MONITOR_THREAD_CPU_TIME(s): %lf\n", probe_thread_cpu_time);
  fprintf(fp, "PROCESS_CPU_TIME(s): %lf\n", process_cpu_time);
  fprintf(fp, "INIT_OVERHEAD: %lf\n", init_overhead);
  fprintf(fp, "CACHE_PERTURBATION_OVERHEAD: %lf\n", cache_overhead);
  fprintf(fp, "PROBE_OVERHEAD: %lf\n", probe_overhead);
  fprintf(fp, "JUMP_OVERHEAD: %lf\n", jump_overhead);
  fprintf(fp, "CUMULATIVE_OVERHEAD: %lf\n", calculated_overheads);
  //  fprintf(fp, "REAL_EXEC_TIME: %lf\n\n", main_thread_cpu_time - calculated_overheads);
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
  //fprintf(stderr, "[ubiprof] PROBE_OVERHEAD(Ticks): %lld\n", g_probe_overheads);
  fprintf(stderr, "[ubiprof] PROBE_OVERHEAD(s): %lf\n", probe_overhead);
  fprintf(stderr, "[ubiprof] JUMP_OVERHEAD(s): %lf\n", jump_overhead);
  fprintf(stderr, "[ubiprof] CUMULATIVE_OVERHEAD(s): %lf\n", calculated_overheads);
  //  fprintf(stderr, "[ubiprof] REAL_EXEC_TIME(s): %lf\n", main_thread_cpu_time - calculated_overheads);
  fprintf(stderr, "[ubiprof] TARGET_OVERHEAD: %.2lf\n", (double) sp_target_overhead);
  fprintf(stderr, "[ubiprof] RUNTIME_OVERHEAD: %.2lf\n", overhead_at_final_epoch);

  // fprintf(stderr, "[ubiprof] EXEC_TIME: %lf\n", main_thread_cpu_time - probe_overhead);

}

*/

#ifndef NO_INIT
// __attribute__((constructor, no_instrument_function)) void initProfiler(); 

  void initProfiler() {

    // Print invoke patching mode
#if defined(INVOKE_PATCH_ASYNC)
    fprintf(stderr, "INVOKE_PATCH_METHOD: INVOKE_PATCH_ASYNC\n");
#endif

    // Print probe toggling mode
#if defined(DISABLE_STRADDLERS)
    fprintf(stderr, "PROBE_TOGGLE_MODE: DISABLE_STRADDLERS\n");
#elif defined(DISABLE_ALL_PROBES)
    fprintf(stderr, "PROBE_TOGGLE_MODE: DISABLE_ALL_PROBES\n");
#else
    fprintf(stderr, "PROBE_TOGGLE_MODE: ENABLE_ALL_PROBES\n");
#endif

    /*
    11/11/15 - BC : Temporarily commented out. Not required for Xmod stuff.

    calibrateTicks();

    struct timespec begin_ts;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &begin_ts);
    uint64_t nanoSecs = begin_ts.tv_sec * 1000000000LL + begin_ts.tv_nsec;

    uint64_t start = nanoSecs * g_TicksPerNanoSec;  

    g_deactivation_count = 0;

    clock_gettime(CLOCK_MONOTONIC, &g_begints);
    */

    char* profiler_type_str = getenv("PROFILER_TYPE");
    if (profiler_type_str != NULL) {
      if (!strcmp(profiler_type_str, "FIXED_BACKOFF")) {
        fprintf(stderr, "[Ubiprof] Intializing the Backoff Profiler..\n");
        g_profiler_type = ProfilerType::BACKOFF;
      } else if (!strcmp(profiler_type_str, "SAMPLING")) {
        fprintf(stderr, "[Ubiprof] Intializing the Sampling Profiler..\n");
        g_profiler_type = ProfilerType::SAMPLING;
      } else if (!strcmp(profiler_type_str, "EMPTY")) {
        fprintf(stderr, "[Ubiprof] Intializing the Empty Profiler..\n");
        g_profiler_type = ProfilerType::EMPTY;
      } else if (!strcmp(profiler_type_str, "ADAPTIVE")) {
        fprintf(stderr, "[Ubiprof] Intializing the Adaptive Profiler..\n");
        g_profiler_type = ProfilerType::ADAPTIVE;
      } else if (!strcmp(profiler_type_str, "MINIMAL_ADAPTIVE")) {
        fprintf(stderr, "[Ubiprof] Intializing the Minimal Adaptive Profiler..\n");
        g_profiler_type = ProfilerType::MINIMAL_ADAPTIVE;
      } else if (!strcmp(profiler_type_str, "MINIMAL_BACKOFF")) {
        fprintf(stderr, "[Ubiprof] Intializing the Minimal Backoff Profiler..\n");
        g_profiler_type = ProfilerType::MINIMAL_BACKOFF;
      } else if (!strcmp(profiler_type_str, "MINIMAL_SAMPLING")) {
        fprintf(stderr, "[Ubiprof] Intializing the Minimal Sampling Profiler..\n");
        g_profiler_type = ProfilerType::MINIMAL_SAMPLING;
      } else {
        fprintf(stderr, "[Ubiprof] Intializing the Minimal Sampling Profiler..\n");
        g_profiler_type = ProfilerType::MINIMAL_SAMPLING;
      }
    } else {
      fprintf(stderr, "[Ubiprof] Intializing the Minimal Sampling Profiler..\n");
      g_profiler_type = ProfilerType::MINIMAL_SAMPLING;
    }

    // Init the suitable profiler depending on enviornment variable
    initializeGlobalProfiler(g_profiler_type); 

    // Calibrate for cache effects
    /*
    if (g_profiler_type == MINIMAL_ADAPTIVE || g_profiler_type == ADAPTIVE) {
      calibrate_cache_effects();
    }
    */

    // ticks end = getticks();

    g_ubiprof_initialized = true;

    fprintf(stderr, "[Ubiprof] Done intializing the profiler..\n\n");

    /*
    11/11/15 - BC : Temporarily commented out. Not required for Xmod stuff.
    fprintf(stderr, "[Ubiprof] Calibrated parameter values per probe\n Indirect Jump Cost : %lu  Cache miss cost : %lu\n",
                        2*g_call_overhead, g_cache_miss_overhead_upper_bound);


    struct timespec end_ts;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &end_ts);
    nanoSecs = end_ts.tv_sec * 1000000000LL + end_ts.tv_nsec;

    uint64_t end = nanoSecs * g_TicksPerNanoSec;  

    g_init_overhead = end - start;
    */

  }
#endif

#ifndef NO_INIT
// This is called by the main thread when the app is shutting down.
// (After main() finishes.)
__attribute__((destructor))
  void destroyProfiler() {

  int retries = 0;
  // Tell probe monitor thread to shutdown 
  if (g_profiler_type == ProfilerType::ADAPTIVE ||
      g_profiler_type == ProfilerType::MINIMAL_ADAPTIVE) {
    g_shutting_down_flag = TERMINATE_REQUESTED;
    while (g_shutting_down_flag != TERMINATED) {
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
      retries++; 
      }
  }
  // BJS HACK 
  fprintf(stderr,"Waited %d ms for monitor to shut down\n", retries * 10);
  
  // 11/11/15 - BC: Temporarily commenting out. Not needed for Xmod stuff.
  // clock_gettime(CLOCK_MONOTONIC, &g_endts);
  // timeSpecDiff(&g_endts, &g_begints);

  
  fprintf(stderr, "\n[Ubiprof] Destroying the profiler..\n");  
  // run the destructor of the profiler instance. 
  delete PROFILER;  

  // 11/11/15 - BC: Temporarily commenting out. Not needed for Xmod stuff.
  // getFinalOverhead needs to happen after the 
  // destructor of the profiler is executed. 
  // getFinalOverhead();

 
  // BJS: Something is still accessing datastructures held within these instances 
  //      here... It crashes! 
  //      The crash is a lookup into a stl datastructure (That I suppose has been freed). 
  //
  //      The destruction of the instrumentor instance as part of the Profiler destructor 
  //      has been turned of as a hack. 
    
  // 11/11/15 - BC: Temporarily commenting out. Not needed for Xmod stuff.
  // fprintf(stderr, "\n[ubiprof] UBIPROF_ELAPSED_TIME : %lf\n", getSecondsFromTS(&g_diff));
  
} 
#endif
