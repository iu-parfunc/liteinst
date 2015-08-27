
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include "patcher.h"
#include "cycle.h"

int NUM_OF_PATCHES = 1000000;

bool g_initialized = false;
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

    g_initialized = true;
  }

  g_num_invocations++;
  return;
}

int main(void) {

  fprintf(stderr, "Measure probe activation and deactivation costs..\n");

  uint64_t activation_cost = 0;
  uint64_t deactivation_cost = 0;

  uint64_t i;
  for (i = 0; i < NUM_OF_PATCHES; i++){

    // At i == 0 we initialize stuff 
    foo(i);

    ticks start = getticks();
    patch_64((void*) g_call_addr, g_inactive_sequence);
    ticks end = getticks();

    assert(end > start);
    deactivation_cost += (end - start);

    start = getticks();
    patch_64((void*) g_call_addr, g_active_sequence);
    end = getticks();

    assert(end > start);
    activation_cost += (end - start);

  }

  fprintf(stderr, "Number of activations: %lu\n", g_num_invocations);
  fprintf(stderr, "Number of deactivations: %lu\n", g_num_invocations);

  // Check if probe modifications happened as intended
  assert(g_num_invocations == NUM_OF_PATCHES);

  // Round figures below since decimal cycles doesn't make much sense
  fprintf(stderr, "Average deactivation cost (cycles) : %lu\n", deactivation_cost /
      NUM_OF_PATCHES);
  fprintf(stderr, "Average activation cost (cycles) : %lu\n", activation_cost / 
      NUM_OF_PATCHES);

  exit(EXIT_SUCCESS);
}
