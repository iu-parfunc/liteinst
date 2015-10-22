#include <stdio.h> 
#include <stdlib.h> 
#include <pthread.h> 
#include <stdint.h> 
#include <stdbool.h> 
#include <unistd.h> 
#include <time.h> 

#define NUM_SWAPS 5000000
#define NS_PER_SEC 1000000000

/* globals */ 

unsigned long g_f_count = 0; 
unsigned long g_g_count = 0; 

bool g_running = true; 

/* the functions to swap between */ 
void f(void) { 
  
  // printf("f * running\n"); 
  g_f_count ++; 

} 

void g(void) { 
  
  // printf("g **** running\n"); 
  g_g_count ++; 
  
}

/* the pointer to swap */ 
void (*g_func_pointer)(void) = f; 

void* swapper(void *arg) {

  for (int i = 0; i < NUM_SWAPS; ++i ) { 
    if (i % 2 == 0) { 
      /* swap for g */ 
      //while(!__sync_bool_compare_and_swap(&g_func_pointer,f,g)); 
      __atomic_store_n(&g_func_pointer,g,__ATOMIC_SEQ_CST); 

    } else { 
      //while(!__sync_bool_compare_and_swap(&g_func_pointer,g,f)); 
      __atomic_store_n(&g_func_pointer,f,__ATOMIC_SEQ_CST); 
    }
  }
  
  g_running = false; 
  
} 


int main(int argc, char **argv){ 
  
  struct timespec t1; 
  struct timespec t2; 
  
  
  pthread_t swap_thread; 
  

  pthread_create(&swap_thread, 
		 NULL, 
		 swapper, 
		 NULL); 
		 
  
  
  
  
  
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t1); 
  while (g_running) { 
   
    g_func_pointer(); 
  }
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t2); 
  
  
  unsigned long elapsed_ns = ( (t2.tv_sec * NS_PER_SEC + t2.tv_nsec) - 
                               (t1.tv_sec * NS_PER_SEC + t1.tv_nsec)); 
  
  
  pthread_join(swap_thread,NULL); 

  printf("DONE!\n") ; 
  unsigned long tot_calls = g_f_count + g_g_count;
  printf(" calls to f: %ld\n", g_f_count);
  printf(" calls to g: %ld\n", g_g_count);
  printf(" calls total: %ld\n", tot_calls); 
  printf(" time elapsed: %ld\n", elapsed_ns); 
  printf(" calls per ns: %f\n", (double)tot_calls/elapsed_ns);
  printf(" swaps per ns: %f\n", (double)NUM_SWAPS/elapsed_ns); 
  printf(" swap epoch: %f\n", (double)elapsed_ns/NUM_SWAPS); 
  
  return 0; 
}
