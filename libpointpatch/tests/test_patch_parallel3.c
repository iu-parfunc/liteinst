/* 
   Test desc: 
   Multiple threads executes a call site, while it is being 
   changed. 

   NUM_RUNNERS threads execute a loop running foo ITERS times 
   1  thread replaces foo call site with a bar call.  
   1  thread replaces foo call site with a foo call. 
   

   Test is considered successful if the both 
   foo and bar are called. 


   **** THIS DOES NOT HOLD WITH THE THREADSAFE PATCHER **** 
   Test is considered successful if the sum of number of 
   times foo and bar is invoked is equal to the 
   ITERS * NUM_RUNNERS. 

   
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

#define ITERS 1000000
#define NUM_RUNNERS 4

unsigned long g_foo_val[NUM_RUNNERS] = {0}; 
unsigned long g_bar_val[NUM_RUNNERS] = {0}; 

/* control */
volatile int g_running = true; 
volatile bool g_first_run = true;


/* patch info */
uint64_t g_call_addr = 0; 
uint64_t g_orig_call = 0;  /* original instr */
uint64_t g_call_bar_patch = 0;      /* replacement isntr sequence */


void bar(int id) {
  
  g_bar_val[id]++; 
  
}


void foo(int id) { 
  
  
  if (g_first_run) { 
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
  
  //printf("Running foo %d\n", apa); 

  g_foo_val[id]++;
} 



void activator(int *arg) { 
  
  while (g_first_run); /* wait until setup phase is done */ 
  
  while(g_running) { 
    
    patch_64((void*)g_call_addr, g_orig_call);
    
    /* pthread_yield(); */ 
  }  
} 

void deactivator(int *arg) { 

  while (g_first_run); /* wait until setup phase is done */ 
  
  while (g_running) { 

    patch_64((void*)g_call_addr, g_call_bar_patch);
    
    /* pthread_yield(); */
  }
} 


void runner(int *arg) { 
  int num_iters = 1;
  int id = *arg;

  if (id >= 0) num_iters = ITERS;
    
  for (int i = 0; i < num_iters; i ++) {
    /* the call_site */
    foo(id); 
  

  }

  

}


int main(void) {

  pthread_t thread1, thread2; 

  pthread_t runners[NUM_RUNNERS]; 
  
  int r1,r2; 
  
  unsigned long it = 0; 
  
  
 
  int ids[NUM_RUNNERS];
  /* I hope this call to foo will initialize the call site */
  int id_ = -1; 
  runner(&id_); 



  /* activate modifier threads */ 
  pthread_create(&thread1,
		 NULL, 
		 (void *) activator, 
		 (void *) &r1);

  pthread_create(&thread2,
		 NULL, 
		 (void *) deactivator, 
		 (void *) &r2); 



  printf("Testing parallel updates to a call_site as it is being executed.\n");
 
  for (int i = 0; i < NUM_RUNNERS; i ++) { 
    ids[i] = i;
    pthread_create(&runners[i],
		   NULL, 
		   (void *) runner, 
		   &ids[i]);
  }

  for (int i = 0; i < NUM_RUNNERS; i ++) { 
    pthread_join(runners[i],NULL); 
  }						

  /* then allow the modifier threads to die */ 
  g_running = false; 
  
  pthread_join(thread1, NULL); 
  pthread_join(thread2, NULL); 
  
  unsigned long foo_sum = 0; 
  unsigned long bar_sum = 0; 

  for (int i = 0; i < NUM_RUNNERS; i ++) { 
    foo_sum += g_foo_val[i]; 
    bar_sum += g_bar_val[i]; 
  }
 
  printf("foo: %ld, bar: %ld\nsum: %ld, Targ: %d\n",
	 foo_sum,
	 bar_sum,
	 foo_sum + bar_sum,
	 ITERS * NUM_RUNNERS);
 
  /*   if (foo_sum + bar_sum == ITERS * NUM_RUNNERS) {  */ 

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
