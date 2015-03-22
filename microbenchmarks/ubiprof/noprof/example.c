
#include <stdio.h>
#include <inttypes.h>
// #include <assert.h>
#include <stdlib.h>

typedef unsigned long long ticks;

// __attribute__((no_instrument_function))
static __inline__ ticks getticks(void) {
  unsigned a, d; 
  asm volatile("rdtsc" : "=a" (a), "=d" (d)); 
  return ((ticks)a) | (((ticks)d) << 32); 
}


static __inline__ ticks getstart(void) {
  unsigned cycles_high = 0, cycles_low = 0; 
  asm volatile ("CPUID\n\t"
             "RDTSC\n\t"
             "mov %%edx, %0\n\t"
             "mov %%eax, %1\n\t": "=r" (cycles_high), "=r" (cycles_low)::
             "%rax", "%rbx", "%rcx", "%rdx");
  return ((ticks)cycles_high) | (((ticks)cycles_low) << 32); 
}

static __inline__ ticks getend(void) {
  unsigned cycles_high = 0, cycles_low = 0; 
  asm volatile("RDTSCP\n\t"
         "mov %%edx, %0\n\t"
          "mov %%eax, %1\n\t"
           "CPUID\n\t": "=r" (cycles_high), "=r" (cycles_low)::
           "%rax", "%rbx", "%rcx", "%rdx");
  return ((ticks)cycles_high) | (((ticks)cycles_low) << 32); 
}

__attribute__ ((noinline))
void bar() {

}

int main(int argc, char** argv) {

  long long N = 1000000; 
  N = atoi(argv[1]);
  int i;
  long long bar_total = 0;

  // To warm up stuff
  /*
  for (i = 0; i < N; i++) {
    bar();
  }
  */

  ticks start = 0, end = 0;

  for (i = 0; i < N; i++) {
    start = getstart();
    bar();
    end = getend();
    /*
    if (end < start) {
        printf("start : %llu  end : %llu diff : %lld\n", start, end , ((long long int)end - start));
    }
    assert(end > start);
    */
    bar_total += (end - start);
  } 

  fprintf(stdout, "Total invocations : %lld\n", N);
  fprintf(stdout, "[regular] bar overhead : %lf\n", ((double)bar_total/ N));

  start = getstart();
  for (i = 0; i < N; i++) {
    bar();
  } 
  end = getend();

  /*
  if (end < start) {
    printf("start : %llu  end : %llu diff : %lld\n", start, end, ((long long int)end - start));
  }
  assert(end > start);
  */

  bar_total = (end - start);

  fprintf(stdout, "[Loop] bar overhead : %lf\n", ((double)bar_total/ N));

  return 0;

}
