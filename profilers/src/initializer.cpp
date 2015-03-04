
#include "profiler.hpp"
#include "fprofiler.hpp"

#include <stdlib.h>
#include <string.h>
#include "sprofiler.hpp"

#define FIXED_BACKOFF 0
#define SAMPLING 1
#define EMPTY 2

/* Global to hold the profiler type */
int profiler_type = 0;

struct timespec g_begints;
struct timespec g_endts;
struct timespec g_diff;

extern void print_probe_info();

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

  double probe_overhead = getSecondsFromTicks(g_probe_overheads);
  double jump_overhead = getSecondsFromTicks(call_overhead);

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
      fprintf(stderr, "Error obtaining main thread cpu time ..\n");
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
  double calculated_overheads = probe_overhead + jump_overhead;

  fprintf(stderr, "\n\n\n\n");
  fprintf(stderr, "[ubiprof] APPLICATION_CPU_TIME (s): %lf\n", main_thread_cpu_time);
  fprintf(stderr, "[ubiprof] MONITOR_THREAD_CPU_TIME (s): %lf\n", probe_thread_cpu_time);
  fprintf(stderr, "[ubiprof] PROCESS_CPU_TIME (s): %lf\n", process_cpu_time);
  // fprintf(stderr, "[ubiprof] PROCESS_OVERHEAD_DELTA: %lf\n", process_overhead_delta);

  fprintf(stderr, "[ubiprof] PROBE_OVERHEAD: %lf\n", probe_overhead);
  fprintf(stderr, "[ubiprof] JUMP_OVERHEAD: %lf\n", jump_overhead);
  fprintf(stderr, "[ubiprof] CUMULATIVE_OVERHEAD: %lf\n", calculated_overheads);
  fprintf(stderr, "[ubiprof] REAL_EXEC_TIME: %lf\n", main_thread_cpu_time - calculated_overheads);
  fprintf(stderr, "[ubiprof] EXEC_TIME: %lf\n", main_thread_cpu_time - probe_overhead);

}

__attribute__((constructor))
  void initProfiler() {

    clock_gettime(CLOCK_MONOTONIC, &g_begints);

    calibrateTicks();

    char* profiler_type_str = getenv("PROFILER_TYPE");
    if (profiler_type_str != NULL) {
      if (!strcmp(profiler_type_str, "FIXED_BACKOFF")) {
        fprintf(stderr, "[Ubiprof] Intializing the BackoffProfiler..\n");
        profiler_type = FIXED_BACKOFF;
      } else if (!strcmp(profiler_type_str, "SAMPLING")) {
        fprintf(stderr, "[Ubiprof] Intializing the SamplingProfiler..\n");
        profiler_type = SAMPLING;
      } else if (!strcmp(profiler_type_str, "EMPTY")) {
        fprintf(stderr, "[Ubiprof] Intializing the SamplingProfiler..\n");
        profiler_type = EMPTY;
      } else {
        fprintf(stderr, "[Ubiprof] Intializing the SamplingProfiler..\n");
        profiler_type = SAMPLING;
      }
    } else {
      fprintf(stderr, "[Ubiprof] Intializing the SamplingProfiler..\n");
      profiler_type = SAMPLING;
    }

    // Init the suitable profiler depending on enviornment variable
    Profiler::getInstance(profiler_type);

    fprintf(stderr, "[Ubiprof] Done intializing the profiler..\n\n");

  }

__attribute__((destructor))
  void destroyProfiler() {

    fprintf(stderr, "\n[Ubiprof] Destroying the profiler..\n");
    delete Profiler::getInstance(profiler_type);

    clock_gettime(CLOCK_MONOTONIC, &g_endts);

    timeSpecDiff(&g_endts, &g_begints);

    getFinalOverhead();
    fprintf(stderr, "\n[ubiprof] UBIPROF_ELAPSED_TIME : %lf\n", getSecondsFromTS(&g_diff));

  } 
