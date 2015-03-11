
#ifndef _PROFILER_HPP_
#define _PROFILER_HPP_

#include "../../api/ubiprof.hpp"
#include "../../common/include/cycle.h"

/**
 *  Profilers needing to run a monitor (e.g: a background thread) for bookkeeping tasks etc.
 *  need to implement this
 * */

class Monitorable {

    public:
          virtual void spawnMonitor() = 0;

};

/*
typedef struct TLStatistics {
  uint64_t thread_local_overhead; // Profiling overhead incurred by this thread
  uint64_t thread_local_count;    // Number of samples captured by this thread
  void* func_stats;
} TLStatistics;
*/

// Temporary exports to make overhead statistics visible to finalizer
extern pthread_t g_monitor_thread;
extern uint64_t g_call_overhead; 
extern uint64_t g_TicksPerNanoSec; 
extern uint64_t g_probe_overheads;
extern uint64_t g_probe_count;

#endif /* _PROFILER_HPP_ */
