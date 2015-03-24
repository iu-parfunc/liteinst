
#include <unistd.h>
#include <stdlib.h>

#include <stdio.h>
#include <inttypes.h>
#include <assert.h>
#include <papi.h>
#include <stdlib.h>
#include <limits.h>
#include <time.h>

void __cyg_profile_func_enter(void *this_fn, void *call_site)
    __attribute__((no_instrument_function));
void __cyg_profile_func_exit(void *this_fn, void *call_site)
    __attribute__((no_instrument_function));

typedef unsigned long long ticks;

__attribute__((no_instrument_function))
static __inline__ ticks getstart(void) {
  unsigned cycles_high = 0, cycles_low = 0; 
    asm volatile ("CPUID\n\t"
                  "RDTSC\n\t"
                  "mov %%edx, %0\n\t"
                  "mov %%eax, %1\n\t": "=r" (cycles_high), "=r" (cycles_low)::
                    "%rax", "%rbx", "%rcx", "%rdx");
  return ((ticks)cycles_high << 32) | (((ticks)cycles_low)); 
}

__attribute__((no_instrument_function))
static __inline__ ticks getend(void) {
  unsigned cycles_high = 0, cycles_low = 0; 
  asm volatile("RDTSCP\n\t"
               "mov %%edx, %0\n\t"
               "mov %%eax, %1\n\t"
               "CPUID\n\t": "=r" (cycles_high), "=r" (cycles_low)::
                 "%rax", "%rbx", "%rcx", "%rdx");
  return ((ticks)cycles_high << 32) | (((ticks)cycles_low)); 
}

void calibrate_cache_effects() {

  // Allocate 2 L3 cache sized memory chunks
  // for i..3
  //   Access one of it element by element
  //   PAPI_start();
  //   _cyg_enter(&calibrate_cache_effects, 0)
  //   _cyge_exit(&calibrate_cahce_effects, 0)
  //   PAPI_end();
  // get the median of L3 misses values
  // Acesss on chunk element by element
  // start = getticks()
  // Acess second chunk element by element
  // end = getticks()
  // cache_perturbation_overhead = L3_misses * ((end - start) / L3 cache wordsize)
  // delete memory chunks
  
  int i,j;
  int cache_lines = 0;
  if (sysconf(_SC_LEVEL3_CACHE_LINESIZE) != 0) {
    cache_lines = sysconf(_SC_LEVEL3_CACHE_SIZE) / sysconf(_SC_LEVEL3_CACHE_LINESIZE); 
  } else {
    cache_lines = sysconf(_SC_LEVEL3_CACHE_SIZE) / 8; 
  }

  fprintf(stdout, "Number of cache lines : %d..\n", cache_lines);

  double FUDGE = 1;
  double *a, *b;
  a = (double*) malloc((int)(sizeof(double) * cache_lines * FUDGE));
  b = (double*) malloc((int)(sizeof(double) * cache_lines * FUDGE));
  
  // Initialize PAPI
  int retval;
  int eventSet = PAPI_NULL;
  long long values[] = {0, 0, 0};
  PAPI_event_info_t evinfo;
  PAPI_mh_level_t *L;

  const int eventlist[] = {PAPI_L3_TCM}; // L3 cache misses
  if ((retval = PAPI_library_init(PAPI_VER_CURRENT)) != PAPI_VER_CURRENT) {
    fprintf(stdout, "[Ubiprof] ERROR 1 calibrating for cache effects..\n");
    return;
  }

  if ((retval = PAPI_create_eventset(&eventSet)) != PAPI_OK) {
    fprintf(stdout, "[Ubiprof] ERROR 2 calibrating for cache effects..\n");
    return;
  }

  if (PAPI_add_event(eventSet, eventlist[0]) != PAPI_OK) {
    fprintf(stdout, "[Ubiprof] ERROR 3 calibrating for cache effects..\n");
    return;
  }

  if ((retval = PAPI_start(eventSet)) != PAPI_OK) {
    fprintf(stdout, "[Ubiprof] ERROR 4 calibrating for cache effects..\n");
    return;
  }

  for (i=0; i<3; i++) {
    // Trash the cache
    double sum = 0;
    for (j=0; j<cache_lines*FUDGE; j++) {
      sum += a[j];
    }

    if ((retval = PAPI_reset(eventSet)) != PAPI_OK) { 
      fprintf(stdout, "[Ubiprof] ERROR 6 calibrating for cache effects..\n");
      return;
    }

    __cyg_profile_func_enter((void*)&calibrate_cache_effects, 0); // We don't use the second param at the moment
    __cyg_profile_func_exit((void*)&calibrate_cache_effects, 0); // We don't use the second param at the moment

    if ((retval = PAPI_read(eventSet, &values[i])) != PAPI_OK) {
      fprintf(stdout, "[Ubiprof] ERROR 5 calibrating for cache effects..\n");
      return;
    }

    if ((retval = PAPI_reset(eventSet)) != PAPI_OK) { 
      fprintf(stdout, "[Ubiprof] ERROR 100 calibrating for cache effects..\n");
      return;
    } 
  }

  if ((retval = PAPI_stop(eventSet, NULL)) != PAPI_OK) {
    fprintf(stdout, "[Ubiprof] ERROR 7 calibrating for cache effects..\n");
  }

  if ((retval = PAPI_remove_event(eventSet, eventlist[0])) != PAPI_OK) {
    fprintf(stdout, "[Ubiprof] ERROR 8 calibrating for cache effects..\n");
  }

  if ((retval = PAPI_destroy_eventset(&eventSet)) != PAPI_OK) {
    fprintf(stdout, "[Ubiprof] ERROR 9 calibrating for cache effects..\n");
  }

  fprintf(stdout, "[Ubiprof] cache_misses[0] : %lld cache_misses[1] : %lld cache_misses[2] : %lld\n",
          values[0], values[1], values[2]);
  
  long long cache_misses = 0; 
  for (j=0; j<3; j++) {
    if (cache_misses < values[j]) {
      cache_misses = values[j];
    }
  }

  srand(time(NULL));

  // Find approximately how much time it takes to load a cache line from the memory
  ticks fetch_overhead = 0;
  for (i=0; i<3; i++) {
    // Trash the cache
    double sum = 0;
    for (j=0; j<cache_lines*FUDGE; j++) {
      sum += a[j];
    }

    for (j=0; j<cache_lines*FUDGE; j++) {
      int index = rand() % (int)(cache_lines  * FUDGE); // Trying to thwart the prefetcher here

      // Warmup CPUID
      // getstart();
      // getend();
      // End warmup
      
      ticks start = getstart();
      sum += b[index]; 
      ticks end = getend();
      ticks current = end - start;

      fprintf(stdout, "Current elapsed time : %lld\n", current);

      if (fetch_overhead < current) {
        fetch_overhead = current;
      }
    }
  }

  fprintf(stdout, "[Ubiprof] cache misses %lld fetch overhead %lld\n", cache_misses, fetch_overhead);
  fprintf(stdout, "[Ubiprof] Cache perturbation overhead of instrumentation : %lld\n", 
      cache_misses * fetch_overhead);

}

int main(int argc, char** argv) {

  int N = atoi(argv[1]);

  calibrate_cache_effects(); 

}
