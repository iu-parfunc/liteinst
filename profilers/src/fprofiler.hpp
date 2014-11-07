
#ifndef _FPROFILER_HPP_
#define _FPROFILER_HPP_

#include <unordered_map>
#include <map>
#include "profiler.hpp"

typedef struct BackoffProfilerStat {
  uint16_t func_id;
  uint64_t count;
} BackoffProfilerStat; 

typedef std::map<uint16_t, BackoffProfilerStat*> BackoffProfilerStats;  

class BackoffProfiler : public Profiler {

  public:
    void initialize();
    void dumpStatistics();
    virtual ~BackoffProfiler();

    BackoffProfilerStats* statistics; 
};

#endif /* _FPROFILER_HPP_ */
