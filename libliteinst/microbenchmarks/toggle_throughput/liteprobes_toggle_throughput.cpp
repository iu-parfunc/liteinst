
/*
  Benchmark description:
  Measures toggle throughput for active/ active patching via fastinst API
 */

#include <pthread.h>
#include <limits.h>

#include "liteinst.hpp"
#include "process.hpp"

using namespace liteinst;
using namespace utils::process;

#define NS_PER_S 1000000000
#define PAD 8
// This must be odd:
// This fixes the worst of the imbalance problem, but a problem still remains:
// #define BURST_SIZE 997
// This is much better for balance, but worse for maximum toggle rate:
#define BURST_SIZE 1

#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))
#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))

/* Control */
volatile int g_running = 0;
volatile int g_start = 0;
volatile int g_globally_finished = 0;
unsigned long n_toggles=0;

long **g_foo_addresses;

long num_runners = 1;
long target_rate = 1000000;
double duration = 1.0;

volatile bool done = false;
volatile int64_t foo_count = 0;
volatile int64_t real_count = 0;

ProbeRegistration pr;

ProbeProvider* p;

volatile int worker_ids = 0;

static __thread long thread_foo_count = 0;
static __thread long runner_loop_count = 0;

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

// Instrumented function
__attribute__ ((noinline))
void func() {
}

// Instrumentation function
void foo() {
  thread_foo_count++;
}

void *runner(void *arg) {

  int my_id = __sync_fetch_and_add(&worker_ids,1);
  g_foo_addresses[my_id] = &thread_foo_count;

  while (!g_globally_finished) {

    // Wait until main thread gives go signal
    while (!g_start) ;

    if (g_globally_finished) break;

    thread_foo_count = 0;

    while(g_running) {
      /* the call site that we patch is within fun */

      func();
      // runner_loop_count++;
    }

  }
}

double run_experiment() {
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
        p->activate(pr);
      } else {
        // printf(".");fflush(stdout);
        p->deactivate(pr);
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
  for(int i=0; i<num_runners; i++) {
    // foo_count += __sync_lock_test_and_set( g_foo_addresses[i], 0);
    foo_count += *g_foo_addresses[i];
  }

  printf("\nFinally, here is some human-readable output, not for HSBencher:\n");
  setlocale(LC_NUMERIC, "");
  printf("Number of toggles : %'lu\n", n_toggles);
  printf("Foo count : %'lu\n", foo_count);
  printf("Combined count : %'lu\n", foo_count);
  printf("Runner loop count : %'lu\n", runner_loop_count);

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
    printf("[main] Running with threads : %ld, duration: %lf, toggle_freq: %ld ..\n",
           num_runners, duration, target_rate);
  }

  // Setting up the probe provider and the instrumentation
  p = liteinst::ProbeProvider::initializeGlobalProbeProvider(
      ProviderType::LITEPROBES, nullptr, nullptr);
  InstrumentationProvider i_provider("Counter", foo, 
      nullptr);

  p->registerInstrumentationProvider(i_provider);
  printf("[main] Registered probe provider..\n");

  // Specifying probe coordinates
  Coordinates coords;
  coords.setFunction(liteinst::Function("_Z4funcv")).
    setProbePlacement(ProbePlacement::ENTRY);

  // Register probe meta data and inject them.
  pr = p->registerProbes(coords, "Counter"); 

  Process process;

  assert(pr.getProbedFunctions().size() == 1);

  printf("[main] Registered probes..\n");

  const int trials = 100000;
  // Run a test
  for(int i=0; i<trials; i++) func();
  assert(thread_foo_count == trials);
  thread_foo_count = 0;

  printf("[main] Passed simple test of %d calls with foo instrumentation\n",
      trials);

  g_foo_addresses = new long*[num_runners](); // (sizeof(long), num_runners);

  // Thread fork
  pthread_t runners[num_runners]; 
  int rc;
  int *ids = new int[num_runners];

  g_start = 0;
  g_running = 1;

  for (int i=0; i < num_runners; i++) {
    int rc = pthread_create(&runners[i], NULL, runner, (void*)&ids[i]);
    if (rc) {
      printf("Error creating thread : %d\n", i);
      exit(-1);
    }
  }

  // Spin until all threads have registered their TLS addresses:
  int not_ready = 1;
  while(not_ready) {
    not_ready = 0;
    for(int i=0; i<num_runners; i++) {
      if (g_foo_addresses[i] == 0) not_ready = 1;
    }
  }

  // Then we can RUN
  // ---------------

  run_experiment();
  double elapsed_time = run_experiment();

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
  unsigned long observed_switches_total = 0;
  unsigned long total_foo_calls = 0;

  for (int i = 0; i < num_runners; i ++) {
    // printf("Runner %d switches: %ld\n", i, g_switches[i*PAD]);
    printf("[main] Runner %d foo calls: %'ld\n", i, *g_foo_addresses[i]);
    // min_switches = MIN(min_switches, g_switches[i*PAD]);
    // max_switches = MAX(max_switches, g_switches[i*PAD]);
    min_foo_calls = MIN(min_foo_calls, *g_foo_addresses[i]);
    max_foo_calls = MAX(max_foo_calls, *g_foo_addresses[i]);
    // observed_switches_total += g_switches[i*PAD];
    total_foo_calls += *g_foo_addresses[i];
  }

  printf("\nALL COUNTS ARE REPORTED AS NUM/SEC\n");
  printf("STRADDLE_POINT: 0\n");
  // printf("MINIMUM_SWITCHES: %f\n", min_switches / elapsed_time);
  // printf("MAXIMUM_SWITCHES: %f\n", max_switches / elapsed_time);
  // printf("OBSERVED_SWITCHES_TOTAL: %f\n", observed_switches_total / elapsed_time);
  printf("MINIMUM_FOO_CALLS: %f\n", min_foo_calls / elapsed_time);
  printf("MAXIMUM_FOO_CALLS: %f\n", max_foo_calls / elapsed_time);

  printf("NUMBER_OF_EXECUTERS: %d\n", num_runners);
  printf("TARGET_TIME: %f\n", duration);
  printf("ELAPSED_TIME: %f\n", elapsed_time);
  printf("NUMBER_OF_TOGGLES: %f\n",n_toggles / elapsed_time);
  printf("TOTAL_FOO_CALLS: %f\n", total_foo_calls / elapsed_time);
  printf("TOTAL_CALLS: %f\n", total_foo_calls / elapsed_time);

  printf("SELFTIMED: %f\n", total_foo_calls / elapsed_time);

  delete[] ids;
  delete[] g_foo_addresses;

  exit(EXIT_SUCCESS);

}
