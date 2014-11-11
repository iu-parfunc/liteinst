
#ifndef _SPROFILER_HPP_
#define _SPROFILER_HPP_

#include <unordered_map>
#include <map>
#include "profiler.hpp"

/* Stack entry */

typedef struct InvocationData {
  ticks timestamp;
  uint16_t func_id;
} InvocationData;

/* Statistics related data structures */
typedef struct SamplingProfilerStat {
  uint16_t func_id;
  uint64_t count;
  uint64_t count_at_last_activation;
  bool is_active;
  uint32_t deactivation_count;
  ticks total_time;
  uint64_t lock;
} SamplingProfilerStat; 

typedef struct TLSSamplingProfilerStat {
  uint16_t func_id;
  uint64_t count;
  uint64_t count_at_last_activation;
  uint32_t deactivation_count;
  ticks total_time;

  // Accounting data
  bool is_active;
  ticks start_timestamp;
  uint32_t stack_depth;
  uint64_t limited_count;
  uint64_t ignore_count;
  InvocationData invocation_stack[20];
} TLSSamplingProfilerStat;

typedef std::map<uint16_t, SamplingProfilerStat*> SamplingProfilerStats;  // Global statistics table
typedef std::map<uint16_t, TLSSamplingProfilerStat*> TLSSamplingProfilerStats; // Thread local statistics table 

class SamplingProfiler : public Profiler, public Monitorable {

  public:
    void initialize();
    void spawnMonitor();
    void dumpStatistics();
    void registerThreadStatistics(TLSSamplingProfilerStats* stats);
    int getThreadCount();
    TLSSamplingProfilerStats** getThreadStatistics();
    virtual ~SamplingProfiler();

    SamplingProfilerStats* statistics; 

  private:
    TLSSamplingProfilerStats** tls_stats;
    int thread_counter = 0; // Number of threads running
    uint64_t sample_size = 10000; // Size of one sample
    uint64_t epoch_period = 10; // Monitor thread sleep period between checks in milliseconds
};

#endif /* _SPROFILER_HPP_ */
