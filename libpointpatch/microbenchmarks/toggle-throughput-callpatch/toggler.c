/*

   The master thread is the toggler. First the master thread forks the
    worker threads and then waits a bit. It then sets a records the
    time (t1) and sets a flag in memory (start).  N executor threads
    spin until they see the flag set, then they begin executing the
    patch site in a loop as fast as possible, checking a stop flag
    in-between calls.  The master thread checks the rdtsc time between
    each toggle, when the desired time has elapsed since t1 (1
    second?), it sets the stop flag.

   So over that approximately one second interval, you get three numbers:

    number of calls to f
    number of calls to g
    number of toggles

  And I think those three numbers (plus N) tells us quite a lot about
  the probing or patching implementation.

*/

#include <time.h>
#include <limits.h>
#include <stdio.h>
#include <memory.h>
#include <stdint.h>
#include <stdlib.h>
#include <locale.h>

#include <wait_free.h>

#if defined(__ICC) || defined(__INTEL_COMPILER)

#else
#ifndef __USE_GNU
#define __USE_GNU
#endif
#endif
#include <pthread.h>

#define NS_PER_S 1000000000
#define PAD 8
// Max toggle throughput of 20Mhz:
#define BURST_SIZE 1
// Toggle throughput of 95Mhz:
// #define BURST_SIZE 997
// Toggle throughput of 83MHz:
// #define BURST_SIZE 13


#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))
#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))

static inline double diff_time_ns(struct timespec *t1, struct timespec *t2){
  double diff_ns = (t1->tv_sec * NS_PER_S + t1->tv_nsec) -
    (t2->tv_sec * NS_PER_S + t2->tv_nsec);

  return diff_ns;
}

static inline double diff_time_s(struct timespec *t1, struct timespec *t2){
  return diff_time_ns(t1,t2) / NS_PER_S;
}

unsigned long *g_foo_val;
unsigned long *g_runner_val;

/* control */
volatile int g_running = true;
volatile bool g_first_run = true;
uint64_t g_init_lock = 0;

/* volatile bool start = false;  */
/* volatile bool stop  = false;  */

volatile bool g_collect_data=false;


/* patch info */
uint64_t g_call_addr = 0;
uint64_t g_orig_call = 0;  /* original instr */
uint64_t g_call_bar_patch = 0;      /* replacement isntr sequence */

/* globals */
uint8_t* fun = NULL;
unsigned int start_addr = 0;

void foo(int arg) {

  if (g_first_run) {
    if (__sync_bool_compare_and_swap(&g_init_lock,0,1)) {
	/* do init stuff */
	uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
	g_call_addr = (uint64_t)((uint8_t*)addr - 5);

	init_patch_site((void*)g_call_addr, 8);

	g_orig_call = *(uint64_t*)g_call_addr;

  uint64_t mask = 0xFFFFFF0000000000; // Mask out the CALL instruction
  uint64_t call_masked = (uint64_t) (g_orig_call & mask); // CALL masked out
  uint64_t nop_mask = 0x0000000000441F0F; // Mask with a 5 byte NOP

  g_call_bar_patch = (call_masked | nop_mask);

	g_first_run = false;
      }
  }

  if (g_collect_data) {
    g_foo_val[arg*PAD]++;
  }

}


void runner(int *arg) {
  int value = *arg;
    
  while(g_running){
    /* the call site that we patch is within fun */

    /* Set up an argument in edi for function */
    asm ("mov %0, %%edi"
	 :
	 : "r" (value)
	 : "edi" );

    ((void (*)(void ))&fun[start_addr])();

    if (g_collect_data) {
      g_runner_val[value*PAD]++;
    }
  }
}

// DATA COLLECTION MUST BE OFF 
void runner_(int arg) { 
  
    asm ("mov %0, %%edi"
	 :
	 : "r" (arg)
	 : "edi" );

    ((void (*)(void ))&fun[start_addr])();
  
}


int main(int argc, char** argv) {

  int num_runners = 4;
  pthread_t *runners;
  unsigned long it = 0;
  double duration = 1.0;  /* seconds */
  long target_rate;       /* patches per second */

  printf("Testing parallel updates to a STRADDLING call_site as it is being executed by multiple threads.\n");

  fun=(uint8_t*)malloc(1024);
  memset(fun,0x90,1024);

  /* where within the call should the straddler occur */
  int call_straddler_point = 1;

  if (argc == 1){
    printf("NO ARGS: Running with default settings\n");
  } else if (argc == 5){ /* if there is an argument */
    call_straddler_point = atoi(argv[1]);
    num_runners = atoi(argv[2]);
    duration    = atof(argv[3]);
    target_rate = atol(argv[4]);
  } else {
    printf("INCORRECT ARGS: need <straddlePoint> <numRunners> <duration> <toggleRate> \n");
    exit(EXIT_FAILURE);
  }
  printf("Setting straddler point at %d (distance in byte into the patch site)\n",call_straddler_point);
  printf("Running with %d threads executing the call site\n", num_runners);

  printf("USING: patch_call_64\n");



  /* Allocate per executer data */

  runners = (pthread_t*)malloc(sizeof(pthread_t)*num_runners);

  g_foo_val = (unsigned long*)malloc(sizeof(unsigned long)*num_runners*PAD);
  g_runner_val = (unsigned long*)malloc(sizeof(unsigned long)*num_runners*PAD);

  for (int i = 0; i < num_runners; i ++) {
    g_foo_val[i*PAD] = 0;
    g_runner_val[i*PAD] = 0;
  }


  /* find a straddling position within fun */
  uint64_t fun_address = (uint64_t)fun;
  size_t cache_line_size=sysconf(_SC_LEVEL3_CACHE_LINESIZE);

  /* how many bytes into a cache line does the fun block begin */
  unsigned int fun_offset = fun_address % cache_line_size;
  printf("FUN_OFFSET = %d\n",fun_offset);

  unsigned int closest_straddler_offset = (cache_line_size - fun_offset);

  start_addr = (closest_straddler_offset - 4) - call_straddler_point;
  printf("start_addr = %d\n",start_addr);
  printf("call_instr_offset = %d\n",start_addr + fun_offset + 4);
  printf("cache_line_offset = %d\n",start_addr + fun_offset);


  uint32_t* addr = (uint32_t*)(&fun[start_addr+5]);

  addr[0] = (uint32_t)((uint64_t)( (uint64_t)foo - (uint64_t)&fun[start_addr + 9]) );


  /* generate some code containing a call at a straddling location */
  fun[start_addr] = 0x55;     /* push %rbp */
  fun[start_addr + 1] = 0x48; /* mov %rsp, %rbp */
  fun[start_addr + 2] = 0x89;
  fun[start_addr + 3] = 0xe5;
  fun[start_addr + 4] = 0xe8; /* Call instr */
  /* Address */
  fun[start_addr + 9] = 0x5d; /* pop %rbp */
  fun[start_addr + 10] = 0xc3;

  init_patch_site((void*)&fun[start_addr],1024);


  /* Start the runners */
  int *ids = (int*)malloc(sizeof(int)*num_runners);

  for (int i = 0; i < num_runners; i ++) {
    ids[i] = i;
    pthread_create(&runners[i],
		   NULL,
		   (void *) runner,
		   &ids[i]);
  }

  /* PERFORM TOGGLING HERE */
  unsigned long n_toggles=0;
  struct timespec t1;
  struct timespec t2;


  
  if (num_runners > 0) { 
     while (g_first_run); /* wait until setup phase is done */
  } else {
    runner_(1000); // nonsense argument
    while (g_first_run); /* wait until setup phase is done */
  }

  printf("First run of foo done: initializes\n");

  // Wait for worker threads to come up:
  usleep(100 * 1000); // Tenth of a second.

  int clock_mode = CLOCK_MONOTONIC;
  // int clock_mode = CLOCK_THREAD_CPUTIME_ID;  // Basically similar effect with this.

  for (int warm_up = 0; warm_up < 2; warm_up ++ ){ 
    
    
    // RESET ALL COUNTERS: 

    for (int i = 0; i < num_runners; i ++) {
      g_foo_val[i*PAD] = 0;
      g_runner_val[i*PAD] = 0;
    }
    n_toggles = 0; 

    // PERFORM 

    clock_gettime(clock_mode, &t1);
    // clock_gettime(clock_mode, &t2);
    // printf("Min clock monotonic gap, in nanoseconds: %lf\n", diff_time_ns(&t2,&t1));
    t2 = t1; /* for first check */

    // collect data even in warmup 
    g_collect_data = true;
    
    double tmp_diff = 0;
    bool   p = true;     /* true -> patch foo, false -> patch bar */
    
    // long current_toggles_per_s; 

    
    while ((tmp_diff = diff_time_s(&t2,&t1))  < duration) {

      long current_toggles_per_s = (long) (n_toggles / tmp_diff);

      long deficit = target_rate - current_toggles_per_s;

      // Try not to overshoot our time window by scheduling too large a batch:
      /*
      double remaining_time = duration - tmp_diff;
      long estimated_remaining_toggles;
      if (remaining_time > 0)
      estimated_remaining_toggles = (long)(current_toggles_per_s / remaining_time);
      if (estimated_remaining_toggles < 1)
      estimated_remaining_toggles = 1;
      if (deficit > estimated_remaining_toggles)
      deficit = estimated_remaining_toggles;
      */
      // We can skip all the above logic if we just cap to a reasonable max.
      // estimated_remaining_toggles would be necessary if we had a VERY
      // slow toggler (like dyninst).
      if (deficit > BURST_SIZE) deficit = BURST_SIZE;
      
      // if (deficit <= 0)
      {
	// printf("keeping up!\n");
	// pthread_yield();
	// Creating a big enough pause in the toggler is kind of cheating because
	// it gives the executors room to breath:
	// for(int i=0; i<100000; i++) {  }
      }
      
      // Magically, including this printf increases invoke throughput by 2-3X, while
      // still allowing the toggler to go up to 67MHz.
      // if (deficit > 100) printf("deficit: %ld\n", deficit);
      // if (deficit > 100) printf("\n");   // This does the trick, but not without \n
      // if (deficit > 100) fflush(stdout); // This doesn't do it.
      
      // We're behind by at least this much... we can catch up as fast
      // as we can with a loop:
      for(; deficit > 0; deficit-- )
	{
	  if (p) {
	    
	    patch_call_64((void*)g_call_addr, g_orig_call);
	    p = false;
	  } else {
	    patch_call_64((void*)g_call_addr, g_call_bar_patch);
	    p = true;
	  }
	  n_toggles++;
	}
      
      clock_gettime(clock_mode, &t2);
    }
    
  } // WARM_UP vs REGULAR RUN LOOP 

  g_running = false;

  for (int i = 0; i < num_runners; i ++) {
    pthread_join(runners[i],NULL);
  }


  free(runners);
  free(ids);


  unsigned long min_foo_calls = ULONG_MAX;
  unsigned long max_foo_calls = 0;
  unsigned long min_runner_calls = ULONG_MAX;
  unsigned long max_runner_calls = 0;
  unsigned long total_foo_calls = 0;
  unsigned long total_calls = 0;

  double t_diff = diff_time_s(&t2,&t1);

  for (int i = 0; i < num_runners; i ++) {
    printf("Runner %d foo calls: %ld\n", i, g_foo_val[i*PAD]);
    printf("Runner %d calls: %ld\n", i, g_runner_val[i*PAD]);

    min_foo_calls = MIN(min_foo_calls, g_foo_val[i*PAD]);
    max_foo_calls = MAX(max_foo_calls, g_foo_val[i*PAD]);
    min_runner_calls = MIN(min_runner_calls, g_runner_val[i*PAD]);
    max_runner_calls = MAX(max_runner_calls, g_runner_val[i*PAD]);
    total_foo_calls += g_foo_val[i*PAD];
    total_calls += g_runner_val[i*PAD];
    
  }
  /* if runners == 0 we get large min_values, these should be zero */ 
  if (num_runners == 0) { 
    min_foo_calls = 0; 
    min_runner_calls = 0; 
  }

  printf("ALL COUNTS ARE REPORTED AS NUM/SEC\n");
  printf("STRADDLE_POINT: %d\n", call_straddler_point);
  printf("MINIMUM_FOO_CALLS: %f\n", min_foo_calls / t_diff);
  printf("MAXIMUM_FOO_CALLS: %f\n", max_foo_calls / t_diff);
  printf("MINIMUM_RUNNER_CALLS: %f\n", min_runner_calls / t_diff);
  printf("MAXIMUM_RUNNER_CALLS: %f\n", max_runner_calls / t_diff);
  printf("NUMBER_OF_EXECUTERS: %d\n", num_runners);
  printf("TARGET_TIME: %f\n", duration);
  printf("ELAPSED_TIME: %f\n", t_diff);

  printf("NUMBER_OF_TOGGLES: %f\n",n_toggles / t_diff);
  printf("TOTAL_FOO_CALLS: %f\n", total_foo_calls / t_diff);
  printf("TOTAL_NOOPS: %f\n", (total_calls - total_foo_calls) / t_diff);
  printf("TOTAL_CALLS: %f\n", total_calls / t_diff);
  printf("SELFTIMED: %f\n", total_calls / t_diff);

  /* printf("\nFinally, here is some human-readable output, not for HSBencher:\n"); */
  /* setlocale(LC_NUMERIC, ""); */
  /* printf("Total callsite toggles: %'ld\n",n_toggles); */
  /* printf("Total calls  completed: %'ld\n", total_foo_calls + total_bar_calls); */
  printf("\nFinally, here is some human-readable output, not for HSBencher:\n");
  setlocale(LC_NUMERIC, "");
  printf("Total callsite toggles: %'lu\n", (unsigned long)(n_toggles / t_diff));
  printf("Total calls  completed: %'lu\n", (unsigned long)(total_calls / t_diff) );



}
