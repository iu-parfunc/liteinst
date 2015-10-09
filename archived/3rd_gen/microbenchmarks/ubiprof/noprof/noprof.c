
#include <stdio.h>
#include <inttypes.h>
#include <assert.h>
#include <stdlib.h>
#include <limits.h>

typedef unsigned long long ticks;

__attribute__((no_instrument_function))
static __inline__ ticks getticks(void) {
  unsigned a, d; 
  asm volatile("rdtsc" : "=a" (a), "=d" (d)); 
  return ((ticks)a) | (((ticks)d) << 32); 
}

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

__attribute__ ((noinline))
void foo(int x ,int y) {

}

__attribute__ ((noinline))
void bar() {

}

int main(int argc, char** argv) {

  // call them once to deactivate instrumentation
  foo(434, 24); 
  bar();

  uint64_t N = 1000000000; 
  N = atoi(argv[1]);
  int i;
  int64_t foo_total = 0;
  int64_t bar_total = 0;

  // To warm up stuff
  /*
  for (i = 0; i < N; i++) {
    foo(234, 454);
    bar();
  }
  */

// #ifdef INDIVIDUAL
  // ticks min = ULLONG_MAX, max=0;
  ticks start = 0, end = 0;
  for (i = 0; i < N; i++) {
    start = getstart();
    foo(234, 454);
    end = getend();

    if (end < start) {
        printf("start : %llu  end : %llu", start, end);
    }
    assert(end > start);
    foo_total += (end - start);
  } 

  for (i = 0; i < N; i++) {
    start = getstart();
    bar();
    end = getend();
    if (end < start) {
        printf("start : %llu  end : %llu\n", start, end);
    }
    assert(end > start);
    bar_total += (end - start);
  } 

  fprintf(stdout, "Total invocations : %lu\n", N);
  fprintf(stdout, "[regular] foo overhead : %ld\n", (foo_total/ N));
  fprintf(stdout, "[regular] bar overhead : %ld\n", (bar_total/ N));
// #endif

// #ifdef AGGREGATE 
  start = getstart();
  for (i = 0; i < N; i++) {
    foo(234, 454);
  } 
  end = getend();
  foo_total = 0;
  foo_total = (end - start);

  start = getstart();
  for (i = 0; i < N; i++) {
    bar();
  } 
  end = getend();
  bar_total = 0;
  bar_total = (end - start);

  fprintf(stdout, "[Loop] foo overhead : %ld\n", (foo_total/ N));
  fprintf(stdout, "[Loop] bar overhead : %ld\n", (bar_total/ N));
// #endif

}
