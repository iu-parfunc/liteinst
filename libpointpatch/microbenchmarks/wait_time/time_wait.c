
#include <stdio.h> 
#include <stdlib.h> 
#include <time.h>
#include <stdint.h> 
#include <limits.h> 

/* ----------------------------------------------------------------- 
   Experiment
   ----------------------------------------------------------------- */    

#define NS_PER_SEC 1000000000
#define N_RUNS 10000000

uint64_t ellapsed_ns(struct timespec *t1, struct timespec *t2) { 
  uint64_t diff = (t2->tv_sec * NS_PER_SEC + t2->tv_nsec) - 
                  (t1->tv_sec * NS_PER_SEC + t1->tv_nsec); 
  
  return diff; 
} 
typedef unsigned long long ticks;
static __inline__ ticks getticks(void)
{
     unsigned a, d;
     asm("cpuid");
     asm volatile("rdtsc" : "=a" (a), "=d" (d));

     return (((ticks)a) | (((ticks)d) << 32));
}

void do_it(void) {
  
  struct timespec t1, t2; 

  
  /*  printf("Iters, ns_tot(AVG), ns_min, min_rdtsc, ns_per_iter\n");*/
  printf("Iters, min_rdtsc\n");
  
  for (int iters = 0; iters <= 100; iters +=5) {
    uint64_t min = ULLONG_MAX; 
    uint64_t sum = 0; 
    uint64_t tmp = 0;
    ticks ticks1 = 0; 
    ticks ticks2 = 0; 
    ticks min_rdtsc = ULLONG_MAX; 
    
    for (int run = 0; run < N_RUNS; run ++) { 
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID,&t1);
      ticks1 = getticks();
    
      for(long i = 0; i < iters; i++) { asm (""); }
      
      ticks2 = getticks();
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID,&t2); 
      tmp = ellapsed_ns(&t1,&t2);
      sum += tmp;
      min = (tmp < min) ? tmp : min;
      
      ticks tick_tmp = (ticks2 - ticks1); 
      min_rdtsc = (tick_tmp < min_rdtsc) ? tick_tmp : min_rdtsc; 

    }
      
  
    /*printf("%d, %f, %ld, %lld, %f\n",iters, (double)sum / N_RUNS
	   ,min , min_rdtsc
	   ,((double)sum / N_RUNS)/iters );*/
    
    printf("%d, %lld\n",iters, min_rdtsc);
    
  }
}




/* ----------------------------------------------------------------- 
   MAIN
   ----------------------------------------------------------------- */    
void main(void) { 


  do_it(); 

  
} 
