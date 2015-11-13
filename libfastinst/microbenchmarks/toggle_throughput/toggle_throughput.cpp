
/*
   Benchmark description:
   Measures toggle throughput for active/ active patching via fastinst API
   */

#include "fastinst.hpp"
#include "cycle.h"
#include "func.hpp"

#include <pthread.h>

#include <string>
#include <cassert>
#include <cstdlib>
#include <time.h>
#include <locale.h>

#define NS_PER_S 1000000000
#define PAD 8

using namespace std;

static inline double diff_time_ns(struct timespec *t1, struct timespec *t2){
  double diff_ns = (t1->tv_sec * NS_PER_S + t1->tv_nsec) -
    (t2->tv_sec * NS_PER_S + t2->tv_nsec);

  return diff_ns;
}

static inline double diff_time_s(struct timespec *t1, struct timespec *t2){
  return diff_time_ns(t1,t2) / NS_PER_S;
}

/* Control */
volatile int g_running = 0;
volatile int g_start = 0;

long num_runners = 1;
long target_rate = 1000000;
double duration = 1.0;

long entry_probe_id = -1;
long exit_probe_id = -1;
long foo_count = 0;
long bar_count = 0;
long FUNC_ID = 0;

string func_mangled = "_Z4funci";

void *runner(void *arg) {

  // Wait until main thread gives go signal
  while (!g_start) ;

  while(g_running){
    /* the call site that we patch is within fun */

    int value = *((int*)arg);

    func(value);
  }
}

/*
   void instrumentation(ProbeArg func_id)
   __attribute__((no_instrument_function));
   void callback(const ProbeMetaData* pmd)
   __attribute__((no_instrument_function));
   */

void foo(ProbeArg func_id) {
  foo_count++;
}

void bar(ProbeArg func_id) {
  bar_count++;
}

void callback(const ProbeMetaData* pmd) {

  if (func_mangled.compare(pmd->func_name) == 0) {
    PROBE_PROVIDER->initialize(pmd->probe_id, FUNC_ID);
    if (pmd->probe_context == ProbeContext::ENTRY) {
      entry_probe_id = pmd->probe_id;
      // fprintf(stderr, "Foo entry probe id : %lu\n", foo_entry_probe_id);
    } else {
      exit_probe_id = pmd->probe_id;
      // fprintf(stderr, "Foo exit probe id : %lu\n", foo_exit_probe_id);
    }

    try {
      PROBE_PROVIDER->activate(pmd->probe_id, foo);
    } catch (int e) {
      fprintf(stderr, "Error while activating probe of function : %s.\n",
          pmd->func_name.c_str());
      exit(EXIT_FAILURE);
    }
  } else {
    assert(false); // This shouldn't happen
  }

}

int main(int argc, char* argv[]) {

  fprintf(stderr, "Benchmark probe toggle throughput..\n");

  if (argc < 4) {
    printf("NOT ENOUGH ARGS, expects 3: threads, duration, toggle_freq \n");
    //    "\nRunning with default settings # threads : %ld # iterations %ld..\n", num_runners, target_rate
    return 1;
  } else {
    num_runners    = atoi(argv[1]);
    duration       = atof(argv[2]);
    target_rate = atoi(argv[3]);
    printf("Running with threads : %ld, duration: %lf, toggle_freq: %ld ..\n",
           num_runners, duration, target_rate);
  }

  ProbeProvider* p;
  try {
    p = initializeGlobalProbeProvider(ProviderType::FINSTRUMENT, callback);
  } catch (int e) {
    fprintf(stderr, "ProbeProvider already initialized. Getting the existing"
        " one..\n");
    p = getGlobalProbeProvider();
  }

  if (p == NULL) {
    fprintf(stderr, "Unable to initialize probe provider..\n");
    exit(EXIT_FAILURE);
  }

  func(0);

  int *ids = new int[num_runners];

  // Turn off the exit probe.
  p->activate(entry_probe_id, foo); // Start in this mode.
  p->deactivate(exit_probe_id);

  g_running = 1;
  pthread_t runners[num_runners];
  int rc;
  for (int i=0; i<num_runners; i++) {
    rc = pthread_create(&runners[i],
        NULL,
        runner, (void *)&ids[i]);
  }

  struct timespec t1;
  struct timespec t2;
  int clock_mode = CLOCK_MONOTONIC;
  clock_gettime(clock_mode, &t1);
  t2 = t1;

  g_start = 1; // Signal to runners.

  unsigned long n_toggles=0;
  int mode = 0;
  double tmp_diff = 0;
  long current_toggles_per_s;
  while ((tmp_diff = diff_time_s(&t2,&t1)) < duration) {

    current_toggles_per_s = (long) (n_toggles / tmp_diff);
    long deficit = target_rate - current_toggles_per_s;
    if (deficit > 1000) deficit = 1000;

    for(; deficit > 0; deficit-- ) {
      if (mode == 0) {
        printf("_");fflush(stdout);
        p->activate(entry_probe_id, bar);
      } else {
        printf(".");fflush(stdout);
        p->activate(entry_probe_id, foo);
      }
      mode = (mode + 1) % 10;
      n_toggles++;
    }

    clock_gettime(clock_mode, &t2);
  }

  g_running = 0; // Signal to runners.

  for (int i = 0; i < num_runners; i ++) {
    pthread_join(runners[i],NULL);
  }

  delete(p);
  delete(ids);

  printf("\nFinally, here is some human-readable output, not for HSBencher:\n");
  setlocale(LC_NUMERIC, "");
  fprintf(stderr, "Number of toggles : %'lu\n", n_toggles);
  fprintf(stderr, "Foo count : %'lu\n", foo_count);
  fprintf(stderr, "Bar count : %'lu\n", bar_count);
  fprintf(stderr, "Combined count : %'lu\n", foo_count + bar_count);

  exit(EXIT_SUCCESS);

}
