
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
  return ((ticks)cycles_high << 32) | (((ticks)cycles_low)); 
}

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
void bar(int x) {
  int i = 0, sum = 0;
  for (i = 0; i < x; i++) {
    sum += i;
  }
}

int main(int argc, char** argv) {

  long long N = 1000000; 
  N = atoi(argv[1]);
  int i;
  long long bar_total = 0;
  int num_bins = 100000/10;
  long long histogram[num_bins];
  long long min = 0;
  long long max = 0;
  long long sum = 0;

  for (i = 0; i < num_bins; i++) {
    histogram[i] = 0;
  }

  // To warm up stuff
  /*
  for (i = 0; i < N; i++) {
    bar();
  }
  */

  ticks start = 0, end = 0;

  for (i = 0; i < N; i++) {
    start = getstart();
    bar(100);
    end = getend();
    /*
    if (end < start) {
        printf("start : %llu  end : %llu diff : %lld\n", start, end , ((long long int)end - start));
    }
    assert(end > start);
    */
    long long elapsed = ((long long) end - start);
    bar_total += elapsed; 
    if (max < elapsed) {
      max = elapsed;
    }

    if (min > elapsed) {
      min = elapsed;
    }

    histogram[elapsed/10]++;
  } 

  fprintf(stdout, "Total invocations : %lld\n", N);
  fprintf(stdout, "[regular] bar overhead : %lf\n", ((double)bar_total/ N));
  fprintf(stdout, "[regular] Min : %lld Max : %lld\n", min, max);

  start = getstart();
  for (i = 0; i < N; i++) {
    bar(100);
  } 
  end = getend();

  /*
  if (end < start) {
    printf("start : %llu  end : %llu diff : %lld\n", start, end, ((long long int)end - start));
  }
  assert(end > start);
  */

  bar_total = ((long long) end - start);

  fprintf(stdout, "[Loop] bar overhead : %lf\n", ((double)bar_total/ N));

  fprintf(stdout, "[regular] Histogram \n");
  for (i = 0; i < num_bins; i++) {
    if (histogram[i] > 0) {
      fprintf(stdout, "%d : %lld\n", i * 10, histogram[i]);
    }
  }

  return 0;

}
