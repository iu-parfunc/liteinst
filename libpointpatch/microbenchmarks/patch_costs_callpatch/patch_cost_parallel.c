
/* Benchmark description : Measures costs of patching for probe activation and
 * deactivation with multiple threads trying to execute the patch concurrently*/

#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

#include "wait_free.h"
#include "cycle.h"

int NUM_OF_PATCHES = 1000000;
int NUM_THREADS = 1;

bool g_initialized = false;
volatile bool g_activator_done = false;
bool g_is_straddler = false;

uint8_t* g_call_addr;
uint64_t g_active_sequence;
uint64_t g_inactive_sequence;
uint64_t g_num_invocations = 0;

/* ----------------------------------------------------------------- */
uint8_t *fun = NULL;
unsigned int start_addr = 0;


/* ----------------------------------------------------------------- */

void foo(void) {

  if (!g_initialized) {
    /* this is the address that foo returns too,
     *        so the call_site is a few bytes before that */
    uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(
        __builtin_return_address(0));
    g_call_addr = ((uint8_t*)addr - 5);

    g_active_sequence = *(uint64_t*)g_call_addr; // ((uint8_t*)addr - 5);
    uint64_t mask = 0xFFFFFF0000000000; // Mask out the CALL instruction
    uint64_t call_masked = (uint64_t) (g_active_sequence & mask); // CALL masked out
    uint64_t nop_mask = 0x0000000000441F0F; // Mask with a 5 byte NOP

    g_inactive_sequence = (call_masked | nop_mask);

    init_patch_site((void*)g_call_addr, 8);

    g_is_straddler = is_straddler_64((void*)g_call_addr);

    g_initialized = true;
  }

  g_num_invocations++;
  return;
}

void invoke_foo(int* id) {

  // If this is the initial call to foo
  if (*id == 0) {
    ((void (*)(void ))&fun[start_addr])();
    //foo(*id);
    return;
  }

  while (!g_activator_done) {
    ((void (*)(void ))&fun[start_addr])();
    //foo(*id);
  }
}

int main(int argc, char* argv[]) {

  fprintf(stderr, "Measure probe activation and deactivation costs with "
      "contention..\n");

  long n_threads = NUM_THREADS; // Set the default
  int call_straddler_point = 0;
  if (argc == 1) {
    printf("NO ARGS: Running with default settings\n");
  }
  else if (argc == 3) {
    n_threads = strtol(argv[1], NULL, 10);
    call_straddler_point = atoi(argv[2]);
  } else {
    printf("Incorrect Arguments\n");
    exit(EXIT_FAILURE);
  }

  /* allocate space for function call */
  fun=(uint8_t*)malloc(1024);
  memset(fun,0x90,1024);


  pthread_t *run_threads;
  run_threads = (pthread_t*)malloc(sizeof(pthread_t)*n_threads);
  uint64_t activation_cost = 0;
  uint64_t deactivation_cost = 0;

  fprintf(stderr,"Running with %ld executor threads and single mutator "
      "threads.\n", n_threads);

  /* -------------------------------------------------------------- */

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

  /* -------------------------------------------------------------- */

  // At first invocation we initialize stuff
  int arg0 = 0;
  invoke_foo(&arg0);

  uint64_t i;
  for (i = 0; i < n_threads; i++) {
    pthread_create(&run_threads[i],
        NULL,
        (void*) invoke_foo,
        (void*) &n_threads);
  }

  bool activation_success = true;
  bool deactivation_success = true;

  while (!g_initialized);

  for (i = 0; i < NUM_OF_PATCHES; i++){

    ticks start = getticks();
    deactivation_success = deactivation_success &&
      patch_call_64((void*) g_call_addr, g_inactive_sequence);
    ticks end = getticks();

    assert(end > start);
    deactivation_cost += (end - start);

    start = getticks();
    activation_success = activation_success &&
      patch_call_64((void*) g_call_addr, g_active_sequence);
    end = getticks();

    assert(end > start);
    activation_cost += (end - start);

  }

  g_activator_done = true;

  for (i = 0; i < n_threads; i++) {
    pthread_join(run_threads[i], NULL);
  }

  fprintf(stderr, "\nIs this a straddler? : %s \n", g_is_straddler ? "TRUE":
      "FALSE");
  fprintf(stderr, "All activations succeeded? %s \n", activation_success ?
      "TRUE" : "FALSE");
  fprintf(stderr, "All deactivations succeeded? %s \n", deactivation_success ?
      "TRUE" : "FALSE");
  fprintf(stderr, "Number of invocations: %lu\n", g_num_invocations);

  fprintf(stderr, "Number of activations: %d\n", NUM_OF_PATCHES);
  fprintf(stderr, "Number of deactivations: %d\n", NUM_OF_PATCHES);

  // Round figures below since decimal cycles doesn't make much sense
  fprintf(stderr, "Average deactivation cost (cycles) : %lu\n", deactivation_cost /
      NUM_OF_PATCHES);
  fprintf(stderr, "Average activation cost (cycles) : %lu\n", activation_cost /
      NUM_OF_PATCHES);


  /* --------------------------------------------------------------- */
  /* --------------------------------------------------------------- */

  fprintf(stdout,"NUM_THREADS: %ld\n", n_threads);
  fprintf(stdout,"THREADS: %ld\n", n_threads);
  fprintf(stdout,"ACTIVATION_COST_AVG: %f\n", (double)activation_cost/NUM_OF_PATCHES);
  fprintf(stdout,"DEACTIVATION_COST_AVG: %f\n", (double)deactivation_cost/NUM_OF_PATCHES);

  exit(EXIT_SUCCESS);
}
