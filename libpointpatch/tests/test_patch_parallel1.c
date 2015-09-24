/* 
    Test desc: 
    tests parallel updates of a call site. 
     - One thread repeatedly writes the original call site code (call to foo). 
     - One thread repeatedly writes a 5 byte nop instruction. 
     
    Test is considered a success if it does not crash. 
   
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

#define ITERS 10000000

unsigned long  g_foo_val = 0; 

/* control */
volatile int g_running = true; 
volatile bool g_first_run = true;
volatile bool activator_done = false; 

/* patch info */
uint64_t g_call_addr = 0; 
uint64_t g_orig_call = 0;  /* original instr */
uint64_t g_patch = 0;      /* replacement isntr sequence */

void foo(int apa) { 
  
  
  if (g_first_run) { 
    /* do init stuff */ 
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

  g_foo_val++;
} 

void activator(int *arg) { 
  
  while (g_first_run); /* wait until setup phase is done */ 
  
  for (int i = 0; i < ITERS; i ++) { 
    
    patch_64((void*)g_call_addr, g_orig_call);
    
    /*pthread_yield(); */
  }
      
  activator_done = true; 
  
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
  
  /* if test reaches this point without crashing it is 
     considered a success */ 
  return 0; 
 

}
