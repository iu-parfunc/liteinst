/* 
    Test desc: 
    tests parallel updates of a call site. 
     - One thread repeatedly writes the original call site code (call to foo). 
     - One thread repeatedly writes a call to bar at that same location. 
     
     foo and bar increments a value each. 
     Test is a success if the foo-value + bar-value == ITERS 
     Iters is the number of times the call site is executed. 
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

unsigned long g_foo_val = 0; 
unsigned long  g_bar_val = 0; 

/* control */
volatile int g_running = true; 
volatile bool g_first_run = true;


/* patch info */
uint64_t g_call_addr = 0; 
uint64_t g_orig_call = 0;  /* original instr */
uint64_t g_call_bar_patch = 0;      /* replacement isntr sequence */


void bar(void) {
  
  g_bar_val++; 
  
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

void foo(void) { 
  
  if (g_first_run) { 
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
  
  //printf("Running foo %d\n", apa); 

  g_foo_val++;
}


int main(int argc, char** argv) {

  pthread_t thread1, thread2; 
  int r1,r2; 
  
  unsigned long it = 0; 

  pthread_create(&thread1,
		 NULL, 
		 (void *) activator, 
		 (void *) &r1);

  pthread_create(&thread2,
		 NULL, 
		 (void *) deactivator, 
		 (void *) &r2); 

   
  printf("Testing parallel updates to a STRADDLING call_site as it is being executed.\n"); 
  
 
  uint8_t* fun=(uint8_t*)malloc(1024); 
  memset(fun,0x90,1024); 

  /* where within the call should the straddler occur */ 
  int call_straddler_point = 1;  

  if (argc == 2){ /* if there is an argument */
    call_straddler_point = atoi(argv[1]);
  } 
  printf("Setting straddler point at %d (distance in byte into the patch site)\n",call_straddler_point); 
  

  /* find a straddling position within fun */
  uint64_t fun_address = (uint64_t)fun; 
  size_t cache_line_size=sysconf(_SC_LEVEL3_CACHE_LINESIZE);

  /* how many bytes into a cache line does the fun block begin */ 
  unsigned int fun_offset = fun_address % cache_line_size; 
  printf("FUN_OFFSET = %d\n",fun_offset);
  
  unsigned int closest_straddler_offset = (cache_line_size - fun_offset);

  unsigned int start_addr = (closest_straddler_offset - 4) - call_straddler_point;
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
  
  
  for (it = 0; it < ITERS; it ++) { 
    /*    foo(); */

    //pthread_mutex_lock(&patch_lock);
    ((void (*)(void ))&fun[start_addr])(); 
    //pthread_mutex_unlock(&patch_lock);
  }
  g_running = false; 
  
  pthread_join(thread1, NULL); 
  pthread_join(thread2, NULL); 
  
  printf("function foo executed %ld times.\n",g_foo_val); 
 
  printf("foo: %ld, bar: %ld\nsum: %ld, Targ: %d\n",
	 g_foo_val,
	 g_bar_val,
	 g_foo_val + g_bar_val,
	 ITERS);
 
  /* if (g_foo_val + g_bar_val == ITERS) { */ 
  if (g_foo_val > 0 && g_bar_val > 0) {
    printf("Success\n");
    return 0; 
  } else { 
    printf("Failure\n");
    return 1; 
  }

  
  /* if test reaches this point without crashing it is 
     considered a success */ 

 

}
