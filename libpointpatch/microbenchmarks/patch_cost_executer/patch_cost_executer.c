
/* Benchmark description : Measures the impact of patching on a function 
   executing a patched call site. */
#define __USE_GNU
#define _GNU_SOURCE

#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h> 
#include <pthread.h>

#include <time.h> 

#include "patcher.h"
#include "cycle.h"
#define NS_PER_S 1000000000
#define NUM_THREADS 1;

bool g_initialized = false;
volatile bool g_activator_done = false;
bool g_is_straddler = false;
bool g_run_activator = true; 
unsigned long g_num_iterations = 100000000; 
unsigned long g_num_activations = 0; 
unsigned long g_activation_count = 0; 

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

  
  
  //g_num_invocations++;
  return;
}


void activator(void *arg) { 
  
  while (!g_initialized); 
  
  while (g_run_activator && g_activation_count < g_num_activations) { 
    /* repeatedly activate */ 
    patch_64((void*) g_call_addr, g_active_sequence);
    g_activation_count ++; 
  }


} 

void invoke_foo(int* id) {

  // If this is the initial call to foo
  if (*id == 0) { 
    ((void (*)(void ))&fun[start_addr])(); 
    return;
  }
 
  for (unsigned long i = 0; i < g_num_iterations; ++i){
    ((void (*)(void ))&fun[start_addr])(); 
  }
  g_run_activator = false; 
}

int main(int argc, char* argv[]) {
  
  unsigned long num_activations; 
  
  fprintf(stderr, "Measure cost induced in thread executing over patch_site\n");

  int n_threads = NUM_THREADS; // Set the default
  int call_straddler_point = 0; 
  if (argc == 1) {
    printf("NO ARGS: Running with default settings\n"); 
  }
  else if (argc == 4) { 
    n_threads = atoi(argv[1]); 
    call_straddler_point = atoi(argv[2]); 
    num_activations = atol(argv[3]); 
  } else { 
    printf("Incorrect Arguments\n"); 
    printf("args: nThreads, straddle_point, nActivations\n"); 
    exit(EXIT_FAILURE); 
  }
  printf("Run configuration: \n"); 
  printf("Exec threads: %d\n", n_threads); 
  printf("Straddle point: %d\n", call_straddler_point); 
  printf("Num activations: %ld\n", num_activations); 
  
  /* allocate space for function call */ 
  fun=(uint8_t*)malloc(1024); 
  memset(fun,0x90,1024); 

  pthread_t activator_thread; 
  pthread_t *run_threads;
  run_threads=(pthread_t*)malloc(sizeof(pthread_t)*n_threads); 
  

  uint64_t activation_cost = 0;
  uint64_t deactivation_cost = 0;

  fprintf(stderr,"Running with %d executor threads.\n", n_threads);

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
  struct timespec t1,t2;   
  
  /* 
     RUN !!!WITH!!! ACTIVATIONS 
  */ 
  fprintf(stdout,"TIMING WITH ACTIVATIONS\n");
  g_run_activator = true;
  g_num_activations = num_activations; 

    
  /* START TIMER */
  clock_gettime(CLOCK_MONOTONIC, &t1); 

  // At first invocation we initialize stuff   
  int arg0 = 0;
  invoke_foo(&arg0);


  for (int i = 0; i < n_threads; i++) {
    pthread_create(&run_threads[i], 
        NULL,
        (void*) invoke_foo,
        (void*) &n_threads);
  }

  pthread_create(&activator_thread, 
		 NULL, 
		 (void*) activator, 
		 NULL); 
		 

  for (int i = 0; i < n_threads; i++) {
    pthread_join(run_threads[i], NULL);
  }
  /* STOP TIMER */
  clock_gettime(CLOCK_MONOTONIC, &t2); 
  
  pthread_join(activator_thread, NULL); 

  unsigned long ns_passed_with_activations = 
    (t2.tv_sec - t1.tv_sec) * NS_PER_S + 
    (t2.tv_nsec - t1.tv_nsec); 

  fprintf(stderr, "\nSTRADDLER: %s \n", g_is_straddler ? "TRUE": 
      "FALSE");  
  /* --------------------------------------------------------------- */
  /* --------------------------------------------------------------- */
  
  double t_act = (double)ns_passed_with_activations / NS_PER_S;
  
  fprintf(stdout,"STRADDLE_POINT: %d\n", call_straddler_point);
  fprintf(stdout,"SELFTIMED: %f\n",t_act); 
  fprintf(stdout,"NUM_ACTIVATIONS: %ld\n",g_activation_count); 
  fprintf(stdout,"NUM_THREADS: %d\n", n_threads); 

  return 0; 
}
