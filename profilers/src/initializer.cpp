
#include "profiler.hpp"
#include "fprofiler.hpp"

#include <stdlib.h>
#include <string.h>

#define FIXED_BACKOFF 0
#define SAMPLING 1

/* Global to hold the profiler type */
int profiler_type = 0;

extern void print_probe_info();

__attribute__((constructor))
  void initProfiler() {

    char* profiler_type_str = getenv("PROFILER_TYPE");
    if (profiler_type_str != NULL) {
      if (!strcmp(profiler_type_str, "FIXED_BACKOFF")) {
        fprintf(stderr, "[Ubiprof] Intializing the BackoffProfiler..\n");
        profiler_type = FIXED_BACKOFF;
      } else if (!strcmp(profiler_type_str, "SAMPLING")) {
        fprintf(stderr, "[Ubiprof] Intializing the SamplingProfiler..\n");
        profiler_type = SAMPLING;
      } else {
        fprintf(stderr, "[Ubiprof] Intializing the SamplingProfiler..\n");
        profiler_type = SAMPLING;
      }
    } else {
      fprintf(stderr, "[Ubiprof] Intializing the SamplingProfiler..\n");
      profiler_type = SAMPLING;
    }

    // Init the suitable profiler depending on enviornment variable
    Profiler::getInstance(profiler_type);

    fprintf(stderr, "[Ubiprof] Done intializing the profiler..\n\n");

  }

__attribute__((destructor))
  void destroyProfiler() {

    fprintf(stderr, "\n[Ubiprof] Destroying the profiler..\n");
    delete Profiler::getInstance(profiler_type);

  } 
