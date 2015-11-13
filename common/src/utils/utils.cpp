
#include <time.h>
#include <utils.hpp>

double g_ticks_per_nano_sec = 0;

// __attribute__((no_instrument_function))
// struct timespec *timeSpecDiff(struct timespec *ts1, struct timespec *ts2); 

void __attribute__ ((noinline)) emptyFunc(uint64_t* a, uint64_t* b) {

}

inline struct timespec timeSpecDiff(struct timespec *ts1, struct timespec *ts2) {
  //static struct timespec ts;
  struct timespec g_diff;
  g_diff.tv_sec = ts1->tv_sec - ts2->tv_sec;
  g_diff.tv_nsec = ts1->tv_nsec - ts2->tv_nsec;

  if (g_diff.tv_nsec < 0) {
    g_diff.tv_sec--;
    g_diff.tv_nsec += 1000000000LL;
  }

  return g_diff;
}

inline double calibrateTicks() {
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
  struct timespec tmpts = timeSpecDiff(&endts, &begints);
  // uint64_t millisecElapsed = tmpts->tv_sec * 1000L + tmpts->tv_nsec / 1000000L;
  // g_ticksPerMilliSec = (double) (end - begin) / (double) millisecElapsed;
  uint64_t nanoSecElapsed = tmpts.tv_sec * 1000000000LL + tmpts.tv_nsec;
  double ticksPerNanoSec = (double) (end - begin) / (double) nanoSecElapsed;
  return ticksPerNanoSec; 
}

double utils::getSecondsFromTicks(ticks duration) {
  if (!g_ticks_per_nano_sec) {
    g_ticks_per_nano_sec = calibrateTicks();
  }

  return (double)duration / (g_ticks_per_nano_sec * 1000000000); 
}
