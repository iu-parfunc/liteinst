
/*
  Benchmark description:
  Measures the overhead of the instrumentation mechanism.

  Instrumentation mechanism overhead includes the cost of __cyg_* call and
  return, time spent inside __cyg_* function and cost of instrumentation 
  function call and return. The cost of instrumentation function itself is not
  included since what we measure is the cost of instrumentation mechanism. 

  Statistics are collected for both prolog and epilog instrumentation plus
  aggregated prolog and epilog instrumentation.
   */

#include "fastinst.hpp"
#include <time.h>
#include "cycle.h"

#include <string>
#include <cstdint>
#include <cassert>

using namespace std;

uint64_t NANO = 1000000000;

uint64_t NUM_INVOCATIONS = 10000000;
uint64_t prolog_overhead = 0;
uint64_t epilog_overhead = 0;
uint64_t total_overhead = 0;

extern "C" {
  void __cyg_profile_func_enter(void *this_fn, void *call_site);
  void __cyg_profile_func_exit(void *this_fn, void *call_site);
}

int foo(int x)
    __attribute__((noinline));
void instrumentation(ProbeArg func_id)
  __attribute__((noinline));
void callback(const ProbeMetaData* pmd)
  __attribute__((no_instrument_function));

int64_t diff(timespec start, timespec end) {
  timespec temp;
  if ((end.tv_nsec-start.tv_nsec)<0) {
    temp.tv_sec = end.tv_sec-start.tv_sec-1;
    temp.tv_nsec = NANO + end.tv_nsec-start.tv_nsec;
  } else {
    temp.tv_sec = end.tv_sec-start.tv_sec;
    temp.tv_nsec = end.tv_nsec-start.tv_nsec;
    fprintf(stderr, "temp.tv_sec, temp.tv_nsec : %lu, %lu\n", temp.tv_sec, 
        temp.tv_nsec);
  }
  return temp.tv_sec * NANO + temp.tv_nsec;
}

// Empty instrumentation since we are not measuring the time inside the 
// instrumentation function itself rather the instrumentation mechanism
void instrumentation(ProbeArg func_id) {
}

void callback(const ProbeMetaData* pmd) {

  // If this callback related to foo probes we activate them
  string foo_mangled = "_Z3fooi"; 
  if (foo_mangled.compare(pmd->func_name) == 0) {
    int FOO_FUNC_ID = 0;

    PROBE_PROVIDER->initialize(pmd->probe_id, FOO_FUNC_ID);
    try {
      PROBE_PROVIDER->activate(pmd->probe_id, instrumentation);
    } catch (int e) {
      fprintf(stderr, "Error while activating probe of function : %s.\n",
          pmd->func_name.c_str());
      exit(EXIT_FAILURE);
    }
  }

}

int foo(int x) {
  ticks prolog_start, prolog_end;
  prolog_start = getticks();
  // Simulating cyg call
  // Not using -finstrument since then we don't have control to place
  // timing code before the generated __cyg_ function
  __cyg_profile_func_enter((void*)foo, &&enter_return); // Label as value here 
  enter_return:
  prolog_end = getticks();

  int64_t prolog_elapsed_ns = (prolog_end - prolog_start); 

  // fprintf(stderr, "Prolog elapsed time : %ld\n", prolog_elapsed_ns);
  if (prolog_elapsed_ns < 0) {
    fprintf(stderr, "[BUSTED] Prolog elapsed time : %ld\n", prolog_elapsed_ns);
  }
  assert(prolog_elapsed_ns >= 0);

  prolog_overhead += prolog_elapsed_ns;

  int i = 0, sum = 0;
  for (i = 0; i<1000; i++) {
    sum += i;
  }
  sum = sum >> 2;

  ticks epilog_start, epilog_end;
  epilog_start = getticks();
  // Simulating cyg call
  // Not using -finstrument since then we don't have control to place
  // timing code after the generated __cyg_ function
  __cyg_profile_func_exit((void*)foo, &&exit_return); // Label as value here 
  exit_return:
  epilog_end = getticks();

  int64_t epilog_elapsed_ns = (epilog_end - epilog_start);

  if (epilog_elapsed_ns < 0) {
    fprintf(stderr, "[Busted] Epilog elapsed time : %ld\n", epilog_elapsed_ns);
  }
  assert(epilog_elapsed_ns >= 0);

  epilog_overhead += epilog_elapsed_ns;

  total_overhead += prolog_elapsed_ns + epilog_elapsed_ns; 

  return 0;
}

int main() {

  fprintf(stderr, "Instrumentation overhead microbenchmark..\n");

  ProbeProvider* p;
  try {
    p = initializeGlobalProbeProvider(ProviderType::FINSTRUMENT, callback);
  } catch (int e) {
    fprintf(stderr, "ProbeProvider already initialized. Getting the existing one..\n");
    p = getGlobalProbeProvider();
  }

  if (p == NULL) {
    fprintf(stderr, "Unable to initialize probe provider..\n");
    exit(EXIT_FAILURE);
  }

  for (int i=0; i< NUM_INVOCATIONS; i++) {
    foo(i);
  }

  fprintf(stderr, "\n\n");
  fprintf(stderr, "NUMBER OF INVOCATIONS : %lu\n", NUM_INVOCATIONS);
  fprintf(stderr, "AVERAGE PROLOG INSTRUMENTATION OVERHEAD (cycles) : %lf\n", 
      (double) prolog_overhead / NUM_INVOCATIONS);
  fprintf(stderr, "AVERAGE EPILOG INSTRUMENTATION OVERHEAD (cycles) : %lf\n", 
      (double) epilog_overhead / NUM_INVOCATIONS);
  fprintf(stderr, "AVERAGE AGGREGATE (PROLOG + EPILOG) INSTRUMENTATION OVERHEAD"
      " (cycles) : %lf\n", (double) total_overhead / NUM_INVOCATIONS);

  delete(p);

  exit(EXIT_SUCCESS);

}
