/* 
    Test desc: 
    tests parallel updates of a STRADDLING call site that is being executed 
    by multiple threads in parallel. 
     - One thread repeatedly writes the original call site code (call to foo). 
     - One thread repeatedly writes a5 byte nop at that same location. 
     
    Currently this test is considered a success if it does not crash
    and if foo has been executed at least once. 
    
    This test needs to be improved as we fix the details of the straddler protocol. 
    
*/ 
#include <stdio.h> 
#include <memory.h>
#include <stdint.h> 
#include <stdlib.h>
#include <time.h>

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


unsigned long g_foo_val = 0; 

/* control */
volatile int g_running = true; 
volatile bool g_first_run = true;
uint64_t g_init_lock = 0; 


/* patch info */
uint64_t g_call_addr = 0; 
uint64_t g_orig_call = 0;  /* original instr */
uint64_t g_nop_patch = 0;  /* replacement isntr sequence */

/* globals */ 
uint8_t* fun = NULL; 
unsigned int start_addr = 0;



void activator(int *arg) { 
  struct timespec t;
  struct timespec r; 
  t.tv_sec = 0; 
  t.tv_nsec = 20; 
  
  printf("activator waiting\n"); 
  while (g_first_run); /* wait until setup phase is done */ 
  
  for (int i = 0; i < ITERS; i ++){
    patch_64((void*)g_call_addr, g_orig_call);

    //sched_yield();
    //nanosleep(&t,&r); 
    /* potentially check r for remainder 
       but I dont think that is important for this test */ 
    
    patch_64((void*)g_call_addr, g_nop_patch); 
  }  

  g_running = false; 
} 

void foo(void) { 
  
  if (g_first_run) { 
    if (__sync_bool_compare_and_swap(&g_init_lock,0,1)) { 
      /* do init stuff */ 
      uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
      g_call_addr = (uint64_t)((uint8_t*)addr - 5);
    
      init_patch_site((void*)g_call_addr, 8);
    
      g_orig_call = *(uint64_t*)g_call_addr;
   
      uint64_t keep_mask = 0xFFFFFF0000000000; 
	   
   	   
      uint64_t nop_mask = 0x0000000000441F0F;

      g_nop_patch = (g_orig_call & keep_mask) | nop_mask;
      printf("nop_patch: %lx\n",g_nop_patch);
    

      g_first_run = false; 
    }
  }
 
  g_foo_val++;
}


void runner(int *arg) { 

  while(g_running){
    /* the call site that we patch is within fun */ 
    ((void (*)(void ))&fun[start_addr])(); 
  }

 
} 


int main(int argc, char** argv) {
  
  pthread_t thread1, thread2; 
  int r1,r2; 
  int num_runners = 4; 
  pthread_t *runners; 
  
  unsigned long it = 0; 
   
  printf("Testing parallel updates to a STRADDLING call_site as it is being executed by multiple threads.\n"); 
  printf("Number of iterations: %d\n",ITERS);
  
 
  fun=(uint8_t*)malloc(1024); 
  memset(fun,0x90,1024); 

  /* where within the call should the straddler occur */ 
  int call_straddler_point = 1;
  
  if (argc == 1){ 
    printf("NO ARGS: Running with default settings\n"); 
  } else if (argc == 3){ /* if there is an argument */
    call_straddler_point = atoi(argv[1]);
    num_runners = atoi(argv[2]); 
  } else { 
    printf("INCORRECT ARGS\n"); 
    exit(EXIT_FAILURE); 
  }								       
  
  printf("Setting straddler point at %d (distance in byte into the patch site)\n",call_straddler_point); 
  printf("Running with %d threads executing the call site\n", num_runners);


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
 

  pthread_create(&thread1,
		 NULL, 
		 (void *) activator, 
		 (void *) &r1);
    
  
  /* Start the runners */ 
  runners = (pthread_t*)malloc(sizeof(pthread_t)*num_runners); 
  int *ids = (int*)malloc(sizeof(int)*num_runners);
 
  for (int i = 0; i < num_runners; i ++) { 
    ids[i] = i;
    pthread_create(&runners[i],
		   NULL, 
		   (void *) runner, 
		   &ids[i]);
  }

       
  pthread_join(thread1, NULL); 

  for (int i = 0; i < num_runners; i ++) { 
    pthread_join(runners[i],NULL); 
  }					

  
  printf("function foo executed %ld times.\n",g_foo_val); 
 
  printf("foo: %ld\n",
	 g_foo_val);
 
  /* if (g_foo_val + g_bar_val == ITERS) { */ 
  if (g_foo_val > 0 ) {
    printf("Success\n");
    return 0; 
  } else { 
    printf("Failure\n");
    return 1; 
  }

  
  /* if test reaches this point without crashing it is 
     considered a success */ 

}
