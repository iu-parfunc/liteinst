
/* Benchmark description : Measures costs of patching for probe activation and
 * deactivation with multiple threads trying to exeute the patch concurrently*/

#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <pthread.h>

#include "patcher.h"
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

void foo(int arg) { 

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

void invoke_foo(void* id) {

  // If this is the initial call to foo
  if (id == 0) {
    foo(id);
    return;
  }

  while (!g_activator_done) {
    foo(id);
  }
}

int main(int argc, char* argv[]) {

  fprintf(stderr, "Measure probe activation and deactivation costs with " 
      "contention..\n");

  long n_threads = NUM_THREADS; // Set the default
  if (argc > 1) {
    n_threads = strtol(argv[1], NULL, 10);
  }

  pthread_t run_threads[n_threads];
  uint64_t activation_cost = 0;
  uint64_t deactivation_cost = 0;

  fprintf(stderr, "Running with %ld executor threads and single mutator "
      "thread..\n", n_threads);

  // At first invocation we initialize stuff 
  invoke_foo(0);

  uint64_t i;
  for (i = 0; i < n_threads; i++) {
    pthread_create(&run_threads[i], 
        NULL,
        (void*) invoke_foo,
        (void*) &n_threads);
  }

  bool activation_success = true;
  bool deactivation_success = true;
  for (i = 0; i < NUM_OF_PATCHES; i++){
    
    ticks start = getticks();
    deactivation_success = deactivation_success && 
      patch_64((void*) g_call_addr, g_inactive_sequence);
    ticks end = getticks();

    assert(end > start);
    deactivation_cost += (end - start);

    start = getticks();
    activation_success = activation_success && 
      patch_64((void*) g_call_addr, g_active_sequence);
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
  
  fprintf(stderr, "Number of activations: %lu\n", NUM_OF_PATCHES);
  fprintf(stderr, "Number of deactivations: %lu\n", NUM_OF_PATCHES);

  // Round figures below since decimal cycles doesn't make much sense
  fprintf(stderr, "Average deactivation cost (cycles) : %lu\n", deactivation_cost /
      NUM_OF_PATCHES);
  fprintf(stderr, "Average activation cost (cycles) : %lu\n", activation_cost / 
      NUM_OF_PATCHES);

  exit(EXIT_SUCCESS);
}
