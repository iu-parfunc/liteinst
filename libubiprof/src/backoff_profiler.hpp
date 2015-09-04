
#ifndef _FPROFILER_HPP_
#define _FPROFILER_HPP_

#include <unordered_map>
#include <map>
#include "profiler.hpp"

typedef struct ProbeStatistics {
  uint64_t* epilog_timings; // Histogram of individual epilog instrumentation timings
  uint64_t* prolog_timings; // Histogram of individual prolog instrumentation timings
  uint64_t* probe_timings; // Histogram of probe timings (epilog + prolog)
} ProbeStatistics;

/// Profiler statistics data structure
typedef struct BackoffProfilerStat {
  uint16_t func_id;
  uint64_t count;
  ticks total_time;
  ticks min_time;
  ticks max_time;
  uint32_t deactivation_count;
  bool is_leaf;
  uint64_t lock;
  uint64_t limited_count;
} BackoffProfilerStat; 

/// Thread local version of profiler statistics data
typedef struct TLSBackoffProfilerStat {
  uint16_t func_id;
  uint64_t count;
  ticks total_time;
  ticks min_time;
  ticks max_time;
  bool is_leaf;

  ticks start_timestamp;
  uint32_t stack_depth;
  uint64_t limited_count;
  uint64_t ignore_count;
  InvocationData invocation_stack[20];
} TLSBackoffProfilerStat; 

class BackoffProfiler : public Profiler {

  public:
    BackoffProfiler();
    void initialize();
    void dumpStatistics();
    int registerThreadStatistics(TLStatistics* tls_stat);
    int getThreadCount();
    TLStatistics** getThreadStatistics();
    virtual ~BackoffProfiler();

    BackoffProfilerStat* statistics; 

  protected:
    TLStatistics** tls_stats;
    // TLSBackoffProfilerStat** tls_stats;
    int thread_counter = 0; // Number of threads running
    uint64_t sample_size = 10000; // Size of one sample
};

class MinimalBackoffProfiler : public BackoffProfiler {

  public:
    void initialize();
    void dumpStatistics();
    virtual ~MinimalBackoffProfiler();
    
};

#endif /* _FPROFILER_HPP_ */
