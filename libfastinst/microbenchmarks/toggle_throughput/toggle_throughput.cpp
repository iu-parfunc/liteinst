
/*
   Benchmark description:
   Measures toggle throughput for active/ active patching via fastinst API
   */

#include "fastinst.hpp"
#include "cycle.h"
#include "func.hpp"

#include <limits.h>
#include <pthread.h>

#include <string>
#include <cassert>
#include <cstdlib>
#include <time.h>
#include <locale.h>

#include <unistd.h>

#define NS_PER_S 1000000000
#define PAD 8
// This must be odd:
// This fixes the worst of the imbalance problem, but a problem still remains:
// #define BURST_SIZE 997
// This is much better for balance, but worse for maximum toggle rate:
#define BURST_SIZE 1

#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))
#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))


using namespace std;

static inline double diff_time_ns(struct timespec *t1, struct timespec *t2){
  double diff_ns = (t1->tv_sec * NS_PER_S + t1->tv_nsec) -
    (t2->tv_sec * NS_PER_S + t2->tv_nsec);

  return diff_ns;
}

static inline double diff_time_s(struct timespec *t1, struct timespec *t2){
  return diff_time_ns(t1,t2) / NS_PER_S;
}

// Spin while waiting to give the OS a hint that we want to stay on this core.
void spin_sleep_ms(unsigned long ms) {
  unsigned long ns = ms * 1000 * 1000;
  struct timespec start, cur;
  clock_gettime(CLOCK_MONOTONIC, &start);
  cur = start;
  while (diff_time_ns(&cur, &start) < ns)
    clock_gettime(CLOCK_MONOTONIC, &cur);
}


/* Control */
volatile int g_running = 0;
volatile int g_start = 0;
volatile int g_globally_finished = 0;
unsigned long n_toggles=0;

long **g_foo_addresses;
long **g_bar_addresses;

// // A place to transfer counts that are stolen from the thread local:
// long foo_count = 0;
// long bar_count = 0;

long num_runners = 1;
long target_rate = 1000000;
double duration = 1.0;

long entry_probe_id = -1;
long exit_probe_id = -1;

long FUNC_ID = 0;

volatile int worker_ids = 0;

static __thread long thread_foo_count = 0;
static __thread long thread_bar_count = 0;

long runner_loop_count = 0;

string func_mangled = "_Z4funci";

void *runner(void *arg) {

  int my_id = __sync_fetch_and_add(&worker_ids,1);
  g_foo_addresses[my_id] = &thread_foo_count;
  g_bar_addresses[my_id] = &thread_bar_count;

  while (!g_globally_finished) {

    // Wait until main thread gives go signal
    while (!g_start) ;

    if (g_globally_finished) break;

    thread_foo_count = 0;
    thread_bar_count = 0;

    while(g_running) {
      /* the call site that we patch is within fun */

      int value = *((int*)arg);

      func(value);
      runner_loop_count++;
    }

  }
}

/*
   void instrumentation(ProbeArg func_id)
   __attribute__((no_instrument_function));
   void callback(const ProbeMetaData* pmd)
   __attribute__((no_instrument_function));
   */

void foo(ProbeArg func_id) {
  thread_foo_count++;
}

void bar(ProbeArg func_id) {
  thread_bar_count++;
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

// Returns ELAPSED_TIME
double run_experiment(ProbeProvider* p) {

  // Reset the global state:
  g_start   = 0;
  n_toggles=0;
  runner_loop_count = 0;
  g_running = 1;

  struct timespec t1, t2;
  int clock_mode = CLOCK_MONOTONIC;

  spin_sleep_ms(100); // Tenth of a second... let the worker threads get started.

  // Then GO!
  clock_gettime(clock_mode, &t1);
  g_start = 1; // Signal to runners.

  t2 = t1;
  int mode = 1;
  double tmp_diff = 0;
  long current_toggles_per_s;
  while ((tmp_diff = diff_time_s(&t2,&t1)) < duration) {

    current_toggles_per_s = (long) (n_toggles / tmp_diff);
    long deficit = target_rate - current_toggles_per_s;
    if (deficit > BURST_SIZE) deficit = BURST_SIZE;
    for(; deficit > 0; deficit-- )
    // if (deficit > 1) //
    {
      if (mode == 0) {
        // printf("_");fflush(stdout);
        p->activate(entry_probe_id, bar);
      } else {
        // printf(".");fflush(stdout);
        p->activate(entry_probe_id, foo);
      }
      // mode = (mode + 1) % 10;
      mode = !mode;
      n_toggles++;
    }

    clock_gettime(clock_mode, &t2);
  }
  g_start   = 0;
  g_running = 0; // Signal to runners.... quit it.
  spin_sleep_ms(100); // Tenth of a second...
  // let the worker threads see the signal and go back to waiting.

  long foo_count = 0;
  long bar_count = 0;
  for(int i=0; i<num_runners; i++) {
    // foo_count += __sync_lock_test_and_set( g_foo_addresses[i], 0);
    // bar_count += __sync_lock_test_and_set( g_foo_addresses[i], 0);
    foo_count += *g_foo_addresses[i];
    bar_count += *g_bar_addresses[i];
  }

  printf("\nFinally, here is some human-readable output, not for HSBencher:\n");
  setlocale(LC_NUMERIC, "");
  fprintf(stderr, "Number of toggles : %'lu\n", n_toggles);
  fprintf(stderr, "Foo count : %'lu\n", foo_count);
  fprintf(stderr, "Bar count : %'lu\n", bar_count);
  fprintf(stderr, "Combined count : %'lu\n", foo_count + bar_count);
  fprintf(stderr, "Runner loop count : %'lu\n", runner_loop_count);

  return diff_time_s(&t2,&t1);
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

  // Call it once to make sure things check out.
  func(0);

  assert(thread_foo_count == 2);
  assert(thread_bar_count == 0);
  thread_foo_count = 0;

  // Turn off the exit probe for the whole benchmark:
  p->deactivate(exit_probe_id);

  const int trials = 100000;

  p->activate(entry_probe_id, foo);
  for(int i=0; i<trials; i++) func(0);
  assert(thread_foo_count == trials);
  assert(thread_bar_count == 0);
  thread_foo_count = 0;

  p->activate(entry_probe_id, bar);
  for(int i=0; i<trials; i++) func(0);
  assert(thread_foo_count == 0);
  assert(thread_bar_count == trials);
  thread_bar_count = 0;

  printf("Passed simple test of %d calls in foo and bar mode\n", trials);
  runner_loop_count = 0;

  g_foo_addresses = (long**)calloc(sizeof(long), num_runners);
  g_bar_addresses = (long**)calloc(sizeof(long), num_runners);

  // ----------------- Thread fork -----------------------
  pthread_t runners[num_runners];
  int rc;
  int *ids = new int[num_runners];

  g_start = 0;
  g_running = 1;
  for (int i=0; i<num_runners; i++) {
    rc = pthread_create(&runners[i],
        NULL,
        runner, (void *)&ids[i]);
  }

  // Spin until all threads have registered their TLS addresses:
  int not_ready = 1;
  while(not_ready) {
    not_ready = 0;
    for(int i=0; i<num_runners; i++) {
      if (g_bar_addresses[i] == 0) not_ready = 1;
      if (g_foo_addresses[i] == 0) not_ready = 1;
    }
  }

  // Then we can RUN
  // ---------------

  run_experiment(p);
  double elapsed_time = run_experiment(p);

  g_globally_finished = 1;
  g_start = 1; // Just to let them get out of their waiting loop and see we're finished.

  for (int i = 0; i < num_runners; i ++) {
    pthread_join(runners[i],NULL);
  }
  // ----------------- Threads joined -----------------------

  unsigned long min_switches = ULONG_MAX;
  unsigned long max_switches = 0;
  unsigned long min_foo_calls = ULONG_MAX;
  unsigned long max_foo_calls = 0;
  unsigned long min_bar_calls = ULONG_MAX;
  unsigned long max_bar_calls = 0;
  unsigned long observed_switches_total = 0;
  unsigned long total_foo_calls = 0;
  unsigned long total_bar_calls = 0;

  for (int i = 0; i < num_runners; i ++) {
    // printf("Runner %d switches: %ld\n", i, g_switches[i*PAD]);
    printf("Runner %d foo calls: %'ld\n", i, *g_foo_addresses[i]);
    printf("Runner %d bar calls: %'ld\n", i, *g_bar_addresses[i]);
    // min_switches = MIN(min_switches, g_switches[i*PAD]);
    // max_switches = MAX(max_switches, g_switches[i*PAD]);
    min_foo_calls = MIN(min_foo_calls, *g_foo_addresses[i]);
    max_foo_calls = MAX(max_foo_calls, *g_foo_addresses[i]);
    min_bar_calls = MIN(min_bar_calls, *g_bar_addresses[i]);
    max_bar_calls = MAX(max_bar_calls, *g_bar_addresses[i]);
    // observed_switches_total += g_switches[i*PAD];
    total_foo_calls += *g_foo_addresses[i];
    total_bar_calls += *g_bar_addresses[i];
  }

  printf("\nALL COUNTS ARE REPORTED AS NUM/SEC\n");
  printf("STRADDLE_POINT: 0\n");
  printf("MINIMUM_SWITCHES: %f\n", min_switches / elapsed_time);
  printf("MAXIMUM_SWITCHES: %f\n", max_switches / elapsed_time);
  printf("OBSERVED_SWITCHES_TOTAL: %f\n", observed_switches_total / elapsed_time);
  printf("MINIMUM_FOO_CALLS: %f\n", min_foo_calls / elapsed_time);
  printf("MAXIMUM_FOO_CALLS: %f\n", max_foo_calls / elapsed_time);
  printf("MINIMUM_BAR_CALLS: %f\n", min_bar_calls / elapsed_time);
  printf("MAXIMUM_BAR_CALLS: %f\n", max_bar_calls / elapsed_time);

  printf("NUMBER_OF_EXECUTERS: %d\n", num_runners);
  printf("TARGET_TIME: %f\n", duration);
  printf("ELAPSED_TIME: %f\n", elapsed_time);
  printf("NUMBER_OF_TOGGLES: %f\n",n_toggles / elapsed_time);
  printf("TOTAL_FOO_CALLS: %f\n", total_foo_calls / elapsed_time);
  printf("TOTAL_BAR_CALLS: %f\n", total_bar_calls / elapsed_time);
  printf("TOTAL_CALLS: %f\n", (total_foo_calls + total_bar_calls) / elapsed_time);

  delete(ids);
  delete(p);

  exit(EXIT_SUCCESS);

}
