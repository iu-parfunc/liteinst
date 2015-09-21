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

#define NUM_TRIALS 100
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
  
  
  printf("Done!\n"); 
  return 0;
}
