#define _GNU_SOURCE
#include <stdio.h> 
#include <stdlib.h> 

#include <pthread.h> 
#include <unistd.h>

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))


int p_lock = 0; 
int ready = 0; 

int affinity[2]; 
unsigned long tsc_vals[2]; 

inline unsigned long rdtscp(int *chip, int *core){
  //unsigned long int x;
  
  unsigned a, d, c;
  __asm__ volatile("rdtscp" : "=a" (a), "=d" (d), "=c" (c));
  
  *chip = (c & 0xFFF000)>>12;
  
  *core = c & 0xFFF;
  return ((unsigned long)a) | (((unsigned long)d) << 32);;
}

void *thread(int *arg) { 
  
  int chip; 
  int core; 
  unsigned long t; 

  int aff = affinity[*arg];

  cpu_set_t cpuset;
  CPU_ZERO(&cpuset); 
  CPU_SET(aff,&cpuset); 

  pthread_setaffinity_np(pthread_self(), sizeof(cpu_set_t), &cpuset); 
  printf("Thread %d running\n", *arg); 

   __sync_fetch_and_add(&ready,1); 

  while (ready < 2); 
  
  
  t = rdtscp(&chip,&core); 
  
  while (!__sync_bool_compare_and_swap(&p_lock, 0, 1));
  printf("Initial time\n chip: %d, core: %d, time: %ld\n", chip, core, t); 
  p_lock = 0; 
  
  tsc_vals[*arg] = t; 
  
  
} 


int main(int argc, char **argv) { 
  
  int n_cpus = sysconf(_SC_NPROCESSORS_ONLN);
  printf("NUM CPUS: %d\n", n_cpus);  
   
  
  pthread_t t1; 
  pthread_t t2; 

  if (argc < 3) { 
    printf("Provide arguments #cpu1 #cpu2\n"); 
    exit(EXIT_FAILURE); 
  }
  
  affinity[0] = atoi(argv[1]); 
  affinity[1] = atoi(argv[2]);  
  
  int arg1 = 0; 
  int arg2 = 1; 
  
  
  pthread_create(&t1,NULL, (void*) thread, &arg1); 
  pthread_create(&t2,NULL, (void*) thread, &arg2); 

  pthread_join(t1,NULL); 
  pthread_join(t2,NULL); 

  unsigned long tsc1 = MAX(tsc_vals[0], tsc_vals[1]);
  unsigned long tsc2 = MIN(tsc_vals[0], tsc_vals[1]);
  

  printf("Difference: %lu\n", tsc1 - tsc2);


  return 0; 
}
