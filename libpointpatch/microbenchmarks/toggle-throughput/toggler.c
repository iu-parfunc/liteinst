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

#include <patcher.h>

#if defined(__ICC) || defined(__INTEL_COMPILER) 

#else 
#ifndef __USE_GNU 
#define __USE_GNU   
#endif 
#endif 
#include <pthread.h> 


#define ITERS 500000
#define NS_PER_S 1000000000

#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))
#define MIN(X,Y) ((X) < (Y) ? (X) : (Y)) 

static inline double diff_time_s(struct timespec *t1, struct timespec *t2){

  double diff_ns = (t1->tv_sec * NS_PER_S + t1->tv_nsec) - 
    (t2->tv_sec * NS_PER_S + t2->tv_nsec); 

  return (diff_ns / NS_PER_S); 

}                 


unsigned long *g_foo_val; 
unsigned long *g_bar_val; 
unsigned long *g_switches; 
bool *g_ran_foo_last; 

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

void bar(int arg) {
  
  if (g_ran_foo_last[arg]) {
    g_ran_foo_last[arg] = false; 
    g_switches[arg]++; 
  }


  if (g_collect_data) {
    g_bar_val[arg]++; 
  }
}

void foo(int arg) { 
  
  if (g_first_run) { 
    if (__sync_bool_compare_and_swap(&g_init_lock,0,1)) { 
	/* do init stuff */ 
	uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
	g_call_addr = (uint64_t)((uint8_t*)addr - 5);
	
	init_patch_site((void*)g_call_addr, 8);
	
	g_orig_call = *(uint64_t*)g_call_addr;
	
	
	uint64_t tmp = 0x00000000000000e8;
	
	uint64_t bar_addr = ((uint64_t)bar - (uint64_t)addr); 
	
	uint64_t keep_mask = 0xFFFFFF0000000000; 
	
	printf("call_instr: %lx\n",tmp);
	printf("bar_addr:   %lx\n",bar_addr); 
	
	tmp = (tmp | (bar_addr << 8)) & ~keep_mask;
	printf("call_bar: %lx\n",tmp);
	
	
	g_call_bar_patch = (g_orig_call & keep_mask) | tmp;
	printf("call_bar_patch: %lx\n",g_call_bar_patch);
    
	
	g_first_run = false; 
      }
  }
  
  if (!g_ran_foo_last[arg]) {
    g_ran_foo_last[arg] = true; 
    g_switches[arg]++; 
  }
    
  
  if (g_collect_data) { 
    g_foo_val[arg]++;
  }

}


void runner(int *arg) { 

  while(g_running){ 
    /* the call site that we patch is within fun */ 
    
    int value = *arg;
    
    /* Set up an argument in edi for function */ 
    asm ("mov %0, %%edi" 
	 : 
	 : "r" (value)
	 : "edi" );

    ((void (*)(void ))&fun[start_addr])(); 
  }
} 

int main(int argc, char** argv) {
  
  int num_runners = 4; 
  pthread_t *runners;
  unsigned long it = 0; 
  double duration = 1.0;  /* seconds */  
   
  printf("Testing parallel updates to a STRADDLING call_site as it is being executed by multiple threads.\n");
 
  fun=(uint8_t*)malloc(1024); 
  memset(fun,0x90,1024); 

  /* where within the call should the straddler occur */ 
  int call_straddler_point = 1;
  
  if (argc == 1){ 
    printf("NO ARGS: Running with default settings\n"); 
  } else if (argc == 4){ /* if there is an argument */
    call_straddler_point = atoi(argv[1]);
    num_runners = atoi(argv[2]); 
    duration    = atof(argv[3]); 
  } else { 
    printf("INCORRECT ARGS\n"); 
    exit(EXIT_FAILURE); 
  }								       
  printf("Setting straddler point at %d (distance in byte into the patch site)\n",call_straddler_point); 
  printf("Running with %d threads executing the call site\n", num_runners);

 
  /* Allocate per executer data */ 
   
  runners = (pthread_t*)malloc(sizeof(pthread_t)*num_runners); 

  g_foo_val = (unsigned long*)malloc(sizeof(unsigned long)*num_runners); 
  g_bar_val = (unsigned long*)malloc(sizeof(unsigned long)*num_runners); 
  g_switches = (unsigned long *)malloc(sizeof(unsigned long)*num_runners); 
  g_ran_foo_last = (bool *)malloc(sizeof(bool)*num_runners); 

  for (int i = 0; i < num_runners; i ++) { 
    g_foo_val[i] = 0; 
    g_bar_val[i] = 0; 
    g_switches[i] = 0; 
    g_ran_foo_last[i] = true; 
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
  
  
  while (g_first_run); /* wait until setup phase is done */ 
  printf("First run of foo done: initializes\n");
  
  
  usleep(500); 
  
  
  clock_gettime(CLOCK_MONOTONIC, &t1); 
  t2 = t1; /* for fist check */ 
  g_collect_data = true; 

  
  while (diff_time_s(&t2,&t1)  < duration){
  
    patch_64((void*)g_call_addr, g_orig_call);
    
    patch_64((void*)g_call_addr, g_call_bar_patch);

    n_toggles+=2; 

    clock_gettime(CLOCK_MONOTONIC, &t2); 
  }
 

  g_running = false;
  
  for (int i = 0; i < num_runners; i ++) { 
    pthread_join(runners[i],NULL); 
  }					

  
  free(runners); 
  free(ids); 


  unsigned long min_switches = ULONG_MAX; 
  unsigned long max_switches = 0;
  unsigned long observed_switches_total = 0; 
  for (int i = 0; i < num_runners; i ++) { 
    printf("Runner %d switches: %ld\n", i, g_switches[i]); 
    printf("Runner %d foo calls: %ld\n", i, g_foo_val[i]); 
    printf("Runner %d bar calls: %ld\n", i, g_bar_val[i]); 
    
    min_switches = MIN(min_switches, g_switches[i]);
    max_switches = MAX(max_switches, g_switches[i]); 
    observed_switches_total += g_switches[i];
    
  }
  printf("STRADDLE_POINT: %d\n", call_straddler_point);
  printf("MINIMUM_SWITCHES: %ld\n", min_switches); 
  printf("MAXIMUM_SWITCHES: %ld\n", max_switches); 
  printf("OBSERVED_SWITCHES_TOTAL: %ld\n", observed_switches_total); 
  printf("NUMBER_OF_TOGGLES: %ld\n",n_toggles); 
  printf("NUMBER_OF_EXECUTERS: %d\n", num_runners); 
  printf("TARGET_TIME: %f\n", duration); 
  printf("ELLAPSED_TIME: %f\n", diff_time_s(&t2,&t1)); 

}


