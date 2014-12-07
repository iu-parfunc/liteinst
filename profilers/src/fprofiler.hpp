
#ifndef _FPROFILER_HPP_
#define _FPROFILER_HPP_

#include <unordered_map>
#include <map>
#include "profiler.hpp"

typedef struct BackoffProfilerStat {
  uint16_t func_id;
  uint64_t count;
  ticks total_time;
  uint64_t lock;
} BackoffProfilerStat; 

typedef struct TLSBackoffProfilerStat {
  uint16_t func_id;
  uint64_t count;
  ticks total_time;

  ticks start_timestamp;
} TLSBackoffProfilerStat; 

/*
typedef std::map<uint16_t, BackoffProfilerStat*> BackoffProfilerStats;  
typedef std::map<uint16_t, TLSBackoffProfilerStat*> TLSBackoffProfilerStats; // Thread
*/

class BackoffProfiler : public Profiler {

  public:
    void initialize();
    void dumpStatistics();
    int registerThreadStatistics(TLSBackoffProfilerStat* tls_stat);
    int getThreadCount();
    TLSBackoffProfilerStat** getThreadStatistics();
    virtual ~BackoffProfiler();

    BackoffProfilerStat* statistics; 

  private:
    TLSBackoffProfilerStat** tls_stats;
    int thread_counter = 0; // Number of threads running
    uint64_t sample_size = 10000; // Size of one sample
};

#endif /* _FPROFILER_HPP_ */
