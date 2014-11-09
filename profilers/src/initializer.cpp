
#include "profiler.hpp"
#include "fprofiler.hpp"

#define FIXED_BACKOFF 0
#define SAMPLING 1

extern void print_probe_info();

__attribute__((constructor))
void initProfiler() {

  fprintf(stderr, "Intializing the profiler..\n");
  // Init the suitable profiler depending on enviornment variable
  // Profiler::getInstance(SAMPLING);
  Profiler::getInstance(FIXED_BACKOFF);

  fprintf(stderr, "Done intializing the profiler..\n");

}

__attribute__((destructor))
void destroyProfiler() {

  // print_probe_info();
  fprintf(stderr, "Destroying the profiler..\n");
  // delete Profiler::getInstance(SAMPLING);
  delete Profiler::getInstance(FIXED_BACKOFF);

} 
