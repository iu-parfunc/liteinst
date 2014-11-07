
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

typedef struct SamplingProfilerStat {
  uint16_t func_id;
  uint64_t count;
  uint64_t count_at_last_activation;
  bool is_active;
  uint32_t deactivation_count;
} SamplingProfilerStat; 

typedef struct TLSSamplingProfilerStat {
  uint16_t func_id;
  uint16_t count;
  uint64_t count_at_last_activation;

  uint32_t stack_depth;
  uint64_t limited_count;
  uint64_t ignore_count;
  InvocationData invocation_stack[20];
} TLSSamplingProfilerStat;


typedef std::map<uint16_t, SamplingProfilerStat*> SamplingProfilerStats;  
typedef std::map<uint16_t, TLSSamplingProfilerStat*> TLSSamplingProfilerStats;  

class SamplingProfiler : public Profiler, public Monitorable {

  public:
    void initialize();
    void spawnMonitor();
    void dumpStatistics();
    void registerThreadStatistics(TLSSamplingProfilerStats* stats);
    virtual ~SamplingProfiler();

    SamplingProfilerStats* statistics; 
};

#endif /* _SPROFILER_HPP_ */
