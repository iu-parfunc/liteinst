/* 
   Test desc: 
   Multiple threads executes a call site, while it is being 
   changed. 

   NUM_RUNNERS threads execute a "infinite" loop running foo 
   1  thread replaces foo call site with a bar call ITERS times.  
   1  thread replaces foo call site with a foo call ITERS times. 
   
   Test is considered successful if the both 
   foo and bar are called and the program does not crash. 
   
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

#ifndef ITERS 
#define ITERS 50000000
#endif 

#define NUM_RUNNERS 4

unsigned long *g_foo_val; //[NUM_RUNNERS] = {0}; 
unsigned long *g_bar_val; // [NUM_RUNNERS] = {0}; 

/* control */
volatile int g_running = true; 
volatile bool g_first_run = true;
uint64_t g_init_lock = 0; 
volatile bool activator_done = false; 

/* patch info */
uint64_t g_call_addr = 0; 
uint64_t g_orig_call = 0;  /* original instr */
uint64_t g_call_bar_patch = 0;      /* replacement isntr sequence */


void bar(int id) {
  
  g_bar_val[id]++; 
  
}


void foo(int id) { 

  
  if (g_first_run) {
    if (__sync_bool_compare_and_swap(&g_init_lock,0, 1)){
      printf("foo initialising\n");   
      /* do init stuff */ 
      uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
      g_call_addr = (uint64_t)((uint8_t*)addr - 5);
      
      init_patch_site((void*)g_call_addr, 8);
    
      g_orig_call = *(uint64_t*)g_call_addr;


      uint64_t tmp = 0x00000000000000e8;
	
      uint64_t bar_addr = ((uint64_t)bar - (uint64_t)addr); 
   
      
      uint64_t keep_mask = 0xFFFFFF0000000000; 
      
      tmp = (tmp | (bar_addr << 8)) & ~keep_mask;
      
      g_call_bar_patch = (g_orig_call & keep_mask) | tmp;
      
      g_first_run = false; 
    }
  }
  //printf("Running foo %d\n", apa); 

  g_foo_val[id]++;
} 



void activator(int *arg) { 
  printf("activator waiting\n"); 
  while (g_first_run); /* wait until setup phase is done */ 
  
  for (int i = 0; i < ITERS; i ++) {   
    patch_64((void*)g_call_addr, g_orig_call);
  } 
  activator_done = true; 
} 

void deactivator(int *arg) { 
  printf("deactivator waiting\n"); 
  while (g_first_run); /* wait until setup phase is done */ 
  
  for (int i = 0; i < ITERS; i ++) {
    patch_64((void*)g_call_addr, g_call_bar_patch);
  }

  while (!activator_done); 
  g_running = false; 
  
} 


void runner(int *arg) { 
  int id = *arg;
  
  while (g_running) { 
    /* the call_site */
    foo(id); 
  }
}


int main(int argc, char **argv) {

  pthread_t thread1, thread2; 

  pthread_t *runners; 
  int num_runners = NUM_RUNNERS;
  
  int r1,r2; 
  
  unsigned long it = 0; 
  
  int *ids;

  printf("Testing parallel updates to a call_site as it is being executed.\n");

  /* arguments */ 
  if (argc == 1){
    printf("NO ARGS: Running with default settings\n"); 
  } else if (argc == 2){
    num_runners = atoi(argv[1]); 
  } else { 
    printf("Incorrect args\n"); 
    exit(EXIT_FAILURE); 
  }
  printf("Running with %d threads executing the call site\n", num_runners);

  g_foo_val = (unsigned long*)malloc(sizeof(unsigned long)*num_runners);
  g_bar_val = (unsigned long*)malloc(sizeof(unsigned long)*num_runners);
  memset(g_foo_val,0,sizeof(unsigned long)*num_runners);
  memset(g_bar_val,0,sizeof(unsigned long)*num_runners);

  
  runners = (pthread_t*)malloc(sizeof(pthread_t)*num_runners);
  ids = (int*)malloc(sizeof(int)*num_runners);

  /* activate modifier threads */ 
  pthread_create(&thread1,
		 NULL, 
		 (void *) activator, 
		 (void *) &r1);

  pthread_create(&thread2,
		 NULL, 
		 (void *) deactivator, 
		 (void *) &r2); 
 
  for (int i = 0; i < num_runners; i ++) {
    ids[i] = i;
    pthread_create(&runners[i],
  		   NULL,
  		   (void *) runner,
  		   &ids[i]);
  }
  
   
  for (int i = 0; i < num_runners; i ++) {
    pthread_join(runners[i],NULL);
  }

  pthread_join(thread1, NULL);
  pthread_join(thread2, NULL);


  unsigned long foo_sum = 0; 
  unsigned long bar_sum = 0; 

  for (int i = 0; i < num_runners; i ++) { 
    foo_sum += g_foo_val[i]; 
    bar_sum += g_bar_val[i]; 
  }
 
  printf("foo: %ld, bar: %ld\nsum: %ld\n",
	 foo_sum,
	 bar_sum,
	 foo_sum + bar_sum);
 
  /*   if (foo_sum + bar_sum == ITERS * NUM_RUNNERS) {  */ 

  free(runners); 
  free(ids); 
  free(g_foo_val); 
  free(g_bar_val); 

  if (foo_sum > 0 && bar_sum > 0) {
    printf("Success\n");
    return 0; 
  } else { 
    printf("Failure\n");
    return 1; 
  }

  
  /* if test reaches this point without crashing it is 
     considered a success */ 

 

}
