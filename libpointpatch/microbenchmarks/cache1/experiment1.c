/* 
gcc --std=gnu99 experiment1.c -lpthread -o experiment1.exe
 */ 

#define __USE_GNU
#define _GNU_SOURCE 

#include <time.h> 
#include <stdio.h>
#include <stdlib.h> 
#include <stdint.h> 
#include <stdbool.h>
#include <unistd.h> 
#include <memory.h>

#include <pthread.h> 
#include <sched.h> 

#define NS_PER_S 1000000000

#define NUM_TRIALS 1000000
#define MAX_CORES  64

volatile uint64_t g_array[NUM_TRIALS] = {0};
volatile int g_ready = 0; 
int *g_all_ready; 
int g_num_cores;

volatile uint64_t g_times[NUM_TRIALS][MAX_CORES];


int pin_thread(int core){

  cpu_set_t cpuset; 
  CPU_ZERO(&cpuset); 
  CPU_SET(core,&cpuset); 

  pthread_t current_thread = pthread_self(); 
  return pthread_setaffinity_np(current_thread, sizeof(cpu_set_t), &cpuset);  
} 


bool readers_ready() {
  bool ready = 1; 
  for (int i = 0; i < g_num_cores; i ++) { 
    ready &= g_all_ready[i]; 
  }
  return ready; 
}

void writer(int *arg) { 
  
  struct timespec t; 
  
  pin_thread(*arg * 2); 
  sched_yield(); 
  
  g_all_ready[*arg] = 1; 

  while(!readers_ready()); 
  
  for (int trial = 0; trial < NUM_TRIALS; trial ++) { 

    clock_gettime(CLOCK_MONOTONIC, &t); 
    g_array[trial]=0xFFFFFFFFFFFFFFFF;
    
    g_times[trial][*arg] = t.tv_sec * NS_PER_S + t.tv_nsec;

    /* sleep some usec, before next test */ 
    usleep(10);
  }
  
} 

void reader(int *arg) { 
  struct timespec t;
  
  if (g_array[0] != 0) printf("error"); /* make sure it is in cache */
  
  pin_thread(*arg * 2);
  sched_yield(); 

  g_all_ready[*arg] = 1; 
 
  
  for (int trial = 0; trial < NUM_TRIALS; trial ++) { 
  
    while (g_array[trial] != 0xFFFFFFFFFFFFFFFF); 
    clock_gettime(CLOCK_MONOTONIC, &t); 
    
    g_times[trial][*arg] = t.tv_sec * NS_PER_S + t.tv_nsec;
    
   
  }
  
}

/* -----------------------------------------------------------------
   SORTING and statistics 
   ----------------------------------------------------------------- */ 

int cmp(const void *a, const void *b) {
  return ( *(uint64_t*)a - *(uint64_t*)b); 
}


double median_in_range(int start, int end, uint64_t *data) { 
  double median;

  int num_vals = end - start; 

  if (num_vals % 2 == 0) { /* even number of values */ 
    uint64_t tmp1 = data[start + (num_vals / 2)];  
    uint64_t tmp2 = data[start + (num_vals / 2 + 1)];  
    median = (double)(tmp1 + tmp2) / 2;
  } else { 
    median = (double)data[start + (num_vals / 2)];  
  }
  
  return median; 
} 

void print_stats() { 
  
  int num_vals = NUM_TRIALS*(g_num_cores-1);
  
  uint64_t *flatvals = 
    (uint64_t*)malloc(sizeof(uint64_t)*num_vals);


  
  /* copy and flatten */ 
  int targ_i = 0; 
  for (int i = 0; i < NUM_TRIALS; i ++){
    for (int j = 1; j < g_num_cores; j ++) {
      flatvals[targ_i] = g_times[i][j] - g_times[i][0]; 
      targ_i++; 
    }
  }  

  /* sort time values */ 

  qsort(flatvals, num_vals, sizeof(uint64_t),cmp);
  

  /*  for (int i = 0; i < num_vals; i ++) { 
    printf ("%lu ", flatvals[i]);
    }*/ 


  /* find median */ 
  
  double median = median_in_range(0, num_vals, flatvals);

  /* find Q1 and Q3 */
  double q1 = median_in_range(0,num_vals/2,flatvals); 
  double q3 = median_in_range(num_vals/2,num_vals,flatvals); 
  
  /* find diff_q */ 

  double diff_q = q3 - q1; 
  
  /* find inner fences  */
  double inner_fence_low  = q1 - (diff_q * 1.5); 
  double inner_fence_high = q3 + (diff_q * 1.5); 
  double outer_fence_low  = q1 - (diff_q * 3); 
  double outer_fence_high = q3 + (diff_q * 3); 
  double extreme_fence_low  = q1 - (diff_q * 9); 
  double extreme_fence_high = q3 + (diff_q * 9); 
  

    
  
  
  //  uint64_t q1     = flatvals[mid_index/2]; 
  //uint64_t q3     = flatvals[mid_index + ((num_vals - mid_index) / 2)]; 
  
  printf("Median: %f\n",median); 
  printf("Q1: %f\n", q1); 
  printf("Q3: %f\n", q3); 
  printf("inner_fence_low: %f\n",inner_fence_low); 
  printf("inner_fence_high: %f\n",inner_fence_high);  
  printf("outer_fence_low: %f\n",outer_fence_low); 
  printf("outer_fence_high: %f\n",outer_fence_high); 
  printf("extreme_fence_low: %f\n",extreme_fence_low); 
  printf("extreme_fence_high: %f\n",extreme_fence_high); 

  
  printf("Some of the EXTREME outliers: \n");
  int m = num_vals - 1; 
  int n_outliers = 0; 
  uint64_t val; 

  while (flatvals[m] > extreme_fence_high) { 
    n_outliers ++;
    if (n_outliers % 100 == 0) {
      printf(" %lu ",flatvals[m]); 
    }
    m --;
  }
  printf("\nEXTREME outliers total: %d\n",n_outliers);
  
  double max_val = extreme_fence_low;
  double min_val = extreme_fence_high; 
  double avg_val = 0; 

  double sum = 0; 
  int counter;

  int start = 0; 
  int end = num_vals - 1; 
  
  while (flatvals[start] <= extreme_fence_low) start++; 
  while (flatvals[end] >= extreme_fence_high) end--; 

  
  for (int i = start; i < end; i++) { 
    uint64_t val = (double)flatvals[i];
    if ( val < min_val) min_val = val;
    if ( val > max_val) max_val = val; 
    sum += val; 
    counter++; 
  }
  
  avg_val = (double)sum / counter; 
  
  printf("Min: %f\n", min_val);
  printf("Max: %f\n", max_val);
  printf("Avg: %f\n", avg_val);
  
  
  
  
  free(flatvals); 

}



/* -----------------------------------------------------------------
   MAIN
   ----------------------------------------------------------------- */ 
int main(int argc, char **argv) { 
  g_num_cores = sysconf(_SC_NPROCESSORS_ONLN)/2;
  int lv3_cache_line_size = sysconf(_SC_LEVEL3_CACHE_LINESIZE);
  
  int num_threads = g_num_cores;

  printf("stats:\nncores: %d\nlv3 linesize: %d\n"
	 ,g_num_cores
	 ,lv3_cache_line_size); 

  printf("Starting...\n"); 

  //g_array = (uint64_t*)malloc(sizeof(uint64_t)*(lv3_cache_line_size/8));
  //memset(g_array,0,sizeof(uint64_t)*(lv3_cache_line_size/8));
  
  //g_times = (uint64_t*)malloc(sizeof(uint64_t)*num_threads);
  //memset(g_times,0,sizeof(uint64_t)*num_threads);
  
  g_all_ready = (int*)malloc(sizeof(int)*num_threads);
  memset(g_all_ready,0,sizeof(int)*num_threads);
  

  pthread_t *threads=(pthread_t*)malloc(sizeof(pthread_t)*num_threads);


  int *ids = (int*)malloc(sizeof(int)*num_threads);

  ids[0] = 0; 
  pthread_create(&threads[0],
  		 NULL,
  		 (void *) writer,
  		 &ids[0]);

  for (int i = 1; i < num_threads; i ++) {
    ids[i] = i;
    pthread_create(&threads[i],
  		   NULL,
  		   (void *) reader,
  		   &ids[i]);
  }
  
  for (int i = 0; i < num_threads; i ++) {
    pthread_join(threads[i],NULL);
  }

  
  for (int trial = 0; trial < NUM_TRIALS; trial ++){ 
    printf("Trial %d: ", trial);
    for (int i = 1; i < num_threads; i ++) { 
    
      printf("%lu ",g_times[trial][i] - g_times[trial][0]); 
    }
    printf("\n");
  }
  
  print_stats();
  
  free(threads); 
  free(g_all_ready);
  
  printf("Done!\n"); 
  return 0;
}
