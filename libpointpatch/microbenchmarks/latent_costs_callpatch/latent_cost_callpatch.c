
/* Benchmark description : Measures costs of a disabled patch site with
 * callpatch */ 
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

#include "wait_free.h"
#include "patcher.h"
#include "cycle.h"

bool g_initialized = false;
bool g_is_straddler = false;

uint8_t* g_call_addr;
uint64_t g_active_sequence;
uint64_t g_inactive_sequence;
uint64_t g_num_invocations = 1000000;

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

  return;
}

int main(int argc, char* argv[]) {

  fprintf(stderr, "Measure probe activation and deactivation costs with "
      "contention..\n");

  int call_straddler_point = 0;
  if (argc == 3) {
    call_straddler_point = atoi(argv[1]);
    g_num_invocations = atoi(argv[2]);
  } else {
    printf("Incorrect Arguments\n");
    exit(EXIT_FAILURE);
  }

  /* allocate space for function call */
  fun=(uint8_t*)malloc(1024);
  memset(fun,0x90,1024);

  fprintf(stderr,"Running %lu times with straddler point %d.\n",
      g_num_invocations, call_straddler_point);

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

  // If this is the initial call to foo
  ((void (*)(void ))&fun[start_addr])();

  patch_call_64((void*) g_call_addr, g_inactive_sequence);

  ticks start = getticks();
  for (int i=0; i< g_num_invocations; i++)  {
    ((void (*)(void ))&fun[start_addr])();
    //foo(*id);
  }
  ticks end = getticks();

  ticks callpatch_cost = end - start;

  // Restore the original call
  patch_call_64((void*) g_call_addr, g_active_sequence);

  // Now deactivate with point patch
  patch_64((void*) g_call_addr, g_inactive_sequence);

  start = getticks();
  for (int i=0; i< g_num_invocations; i++)  {
    ((void (*)(void ))&fun[start_addr])();
  }
  end = getticks();

  ticks pointpatch_cost = end - start;


  fprintf(stderr, "\nIs this a straddler? : %s \n", g_is_straddler ? "TRUE":
      "FALSE");
  fprintf(stderr, "Number of invocations: %lu\n", g_num_invocations);

  // Round figures below since decimal cycles doesn't make much sense
  fprintf(stderr, "Callpatch cost (cycles) : %llu\n", callpatch_cost);
  fprintf(stderr, "Pointpatch cost (cycles) : %llu\n", pointpatch_cost);

  assert(callpatch_cost > pointpatch_cost);
  fprintf(stderr, "LATENT_COST: %llu\n", (callpatch_cost - pointpatch_cost)/
      g_num_invocations);

  exit(EXIT_SUCCESS);
}
