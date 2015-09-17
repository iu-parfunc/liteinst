
#ifndef _PROFILER_HPP_
#define _PROFILER_HPP_

#include <assert.h>
#include "ubiprof.hpp"
#include "cycle.h"
#include "globals.hpp"
#include "constants.h"

/**
 *  Profilers needing to run a monitor (e.g: a background thread) for bookkeeping tasks etc.
 *  need to implement this
 * */

class Monitorable {

    public:
          virtual void spawnMonitor() = 0;

};

/* Thread local overhead statistics */
typedef struct TLStatistics {
  uint64_t thread_local_overhead; // Profiling overhead incurred by this thread
  uint64_t thread_local_count;    // Number of samples captured by this thread
  uint64_t prolog_overhead;       // Current entered function's instrumentation prolog overhead.
  bool deactivated;
  void* func_stats;
} TLStatistics;

/* Stack entry */
typedef struct InvocationData {
  ticks timestamp;
  uint16_t func_id;
  uint64_t prolog_leaf_count;
  uint64_t epilog_leaf_count;
  uint64_t leaf_count;
} InvocationData;

enum class RunState { RUNNING, SHUTDOWN_REQUESTED, SHUTDOWN_ACKED, SHUTTING_DOWN };
extern volatile RunState g_ubiprof_state; 
                                         // This flag is used to communicate 
                                         // the fact that Ubiprof is shutting
                                         // down to daemon threads running in
                                         // the background

#endif /* _PROFILER_HPP_ */
