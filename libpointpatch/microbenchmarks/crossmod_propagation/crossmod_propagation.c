/*
    Test desc:

    Tests single update of a call site.

     - Site starts as NOP
     - One thread writes the original call site code (call to foo).
     - N-1 threads execute the code in a tight loop
     - Threads record when they first hit the call

    info:
    Patching with a single 5byte nop or 5 x 1 byte nops make a big difference.
    using 1byte nops is incorrect and crashes.

*/

#include <stdio.h>
#include <memory.h>
#include <stdint.h>
#include <stdlib.h>

#include <patcher.h>

#if defined(__ICC) || defined(__INTEL_COMPILER)

#else
#ifndef __USE_GNU
#define __USE_GNU
#endif
#endif
#include <pthread.h>

// #define ITERS 10000000
#define ITERS 1

unsigned long  g_foo_val = 0;

/* control */
volatile int g_running = true;
volatile bool g_first_run = true;
volatile bool activator_done = false;

/* patch info */
uint64_t g_call_addr = 0;
uint64_t g_orig_call = 0;  /* original instr */
uint64_t g_patch = 0;      /* replacement isntr sequence */

// An array keeping track of the that each core was reached
// with the modified instruction:
// tick* time_reached;

void __attribute__ ((noinline)) foo(int apa) {

  if (g_first_run) {
    /* do init stuff */
    // This is how we learn where the call instruction is.
    uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
    g_call_addr = (uint64_t)((uint8_t*)addr - 5);

    init_patch_site((void*)g_call_addr, 8);

    g_orig_call = *(uint64_t*)g_call_addr;

    uint64_t nop_patch =  0x0000000000441F0F; //0x0000009090909090; //
    uint64_t keep_mask = 0xFFFFFF0000000000;
    g_patch = (g_orig_call & keep_mask) | nop_patch;

    g_first_run = false;
  }

  //printf("Running foo %d\n", apa);

  __sync_fetch_and_add(&g_foo_val,1);
}

void activator(int *arg) {

  while (g_first_run); /* wait until setup phase is done */

  for (int i = 0; i < ITERS; i ++) {
    patch_64((void*)g_call_addr, g_orig_call);
    /*pthread_yield(); */
  }

  activator_done = true;

  // HACK: put some signaling in here.
  for (int i = 0; i < 1000000; i++) {
    g_running = true;
  }
  g_running = false;
}

void deactivator(int *arg) {

  while (g_first_run); /* wait until setup phase is done */

  for (int i = 0; i < ITERS; i ++) {

    patch_64((void*)g_call_addr, g_patch);

    /* pthread_yield(); */
  }

  while (!activator_done);
  g_running = false;
}


void runner(int *arg) {
  if (*arg >= 0)
    printf("Starting runner... %d\n", *arg);
  do {
    // Hacky code to have a single call site but use this from two places:
    if (*arg >= 0 && !g_running) break;
    foo(1000);
  } while(g_running);

  /* while(g_running){ */
  /*   /\* the call site that we patch is within fun *\/ */
  /*   ((void (*)(void ))&fun[start_addr])(); */
  /* } */
}

int main(int argc, char** argv) {
  pthread_t thread1;
  int r1,r2;
  int num_runners = 4;
  pthread_t *runners;

  unsigned long it = 0;
  printf("Number of iterations: %d\n",ITERS);

  if (argc == 1){
    printf("NO ARGS: Running with default settings\n");
  } else if (argc == 2){ /* if there is an argument */
    num_runners = atoi(argv[1]);
  } else {
    printf("INCORRECT ARGS\n");
    exit(EXIT_FAILURE);
  }
  printf("Running with %d threads executing the call site\n", num_runners);

  runners = (pthread_t*)malloc(sizeof(pthread_t)*num_runners);

  /* Start the runners */
  int *ids = (int*)malloc(sizeof(int)*num_runners);

  // Call it once in the active state to initialize:
  g_running = false;
  int fake_id = -1;
  runner(&fake_id);
  g_running = true;
  // Set the initial state:
  patch_64((void*)g_call_addr, g_patch);

  pthread_create(&thread1,
		 NULL,
		 (void *) activator,
		 (void *) &r1);

  for (int i = 0; i < num_runners; i ++) {
    ids[i] = i;
    pthread_create(&runners[i],
		   NULL,
		   (void *) runner,
		   &ids[i]);
  }

  pthread_join(thread1, NULL);

  for (int i = 0; i < num_runners; i ++) {
    pthread_join(runners[i],NULL);
  }

  free(runners);
  free(ids);

  printf("function foo executed %ld times.\n",g_foo_val);

  /* printf("foo: %ld, bar: %ld\nsum: %ld\n", */
  /*        g_foo_val, */
  /*        g_bar_val, */
  /*        g_foo_val + g_bar_val); */

  /* if (g_foo_val + g_bar_val == ITERS) { */
  if (g_foo_val > 0) {
    printf("Success\n");
    return 0;
  } else {
    printf("Failure\n");
    return 1;
  }

  /* if test reaches this point without crashing it is
     considered a success */

}


/*
int main(void) {

  pthread_t thread1, thread2;
  int r1,r2;

  pthread_create(&thread1,
		 NULL,
		 (void *) activator,
		 (void *) &r1);

  pthread_create(&thread2,
		 NULL,
		 (void *) deactivator,
		 (void *) &r2);


  printf("Testing parallel updates to a call_site as it is being executed.\n");

  while(g_running){
    foo(1000);
  }

  pthread_join(thread1, NULL);
  pthread_join(thread2, NULL);

  printf("function foo executed %ld times.\n",g_foo_val);

  return 0;
}
*/
