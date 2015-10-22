#include <stdio.h> 
#include <stdlib.h> 
#include <pthread.h> 
#include <stdint.h> 
#include <stdbool.h> 
#include <unistd.h> 
#include <time.h> 

#include <patcher.h> 

#define NUM_SWAPS 5000000
#define NS_PER_SEC 1000000000

/* globals */ 

unsigned long g_f_count = 0; 
unsigned long g_g_count = 0; 
unsigned long g_elapsed_ns = 0; 

bool g_running = true; 
bool g_first_run = true; 
/* patch info */
uint64_t g_call_addr = 0; 
uint64_t g_call_f = 0;  /* original instr */
uint64_t g_call_g = 0;  /* replacement isntr sequence */


/* the functions to swap between */ 

void g(void) { 
  
  // printf("g **** running\n"); 
  g_g_count ++; 
  
}


void f(void) { 
  if (g_first_run) { 
   
    /* do init stuff */ 
    uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
    g_call_addr = (uint64_t)((uint8_t*)addr - 5);
    
    init_patch_site((void*)g_call_addr, 8);
	
    g_call_f = *(uint64_t*)g_call_addr;
    
    
    uint64_t tmp = 0x00000000000000e8;
    
    uint64_t addr_g = ((uint64_t)g - (uint64_t)addr); 
    
    uint64_t keep_mask = 0xFFFFFF0000000000; 
        
    tmp = (tmp | (addr_g << 8)) & ~keep_mask;
    
    g_call_g = (g_call_f & keep_mask) | tmp;
    printf("call_bar_patch: %lx\n",g_call_g);
    
    
    g_first_run = false; 
  }
  // printf("f * running\n"); 
  g_f_count ++; 

} 


void* swapper(void *arg) {
  struct timespec t1; 
  struct timespec t2; 



  while (g_first_run); 
  
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t1); 
  for (int i = 0; i < NUM_SWAPS; ++i ) { 
    if (i % 2 == 0) { 
      patch_64((void*)g_call_addr, g_call_g);
    } else { 
      patch_64((void*)g_call_addr, g_call_f);
    }
  }
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t2); 

  g_elapsed_ns = ( (t2.tv_sec * NS_PER_SEC + t2.tv_nsec) - 
		   (t1.tv_sec * NS_PER_SEC + t1.tv_nsec)); 
  
  
  g_running = false; 
  
} 


int main(int argc, char **argv){ 
  
   
  pthread_t swap_thread; 
  

  pthread_create(&swap_thread, 
		 NULL, 
		 swapper, 
		 NULL); 
		 
  
  
  while (g_running) { 
   
    f(); 
  }

  
  

  
  pthread_join(swap_thread,NULL); 

  printf("DONE!\n") ; 
  unsigned long tot_calls = g_f_count + g_g_count;
  printf(" calls to f: %ld\n", g_f_count);
  printf(" calls to g: %ld\n", g_g_count);
  printf(" calls total: %ld\n", tot_calls); 
  printf(" time elapsed: %ld\n", g_elapsed_ns); 
  printf(" calls per ns: %f\n", (double)tot_calls/g_elapsed_ns);
  printf(" swaps per ns: %f\n", (double)NUM_SWAPS/g_elapsed_ns); 
  printf(" swap epoch: %f\n", (double)g_elapsed_ns/NUM_SWAPS); 
  
  return 0; 
}
