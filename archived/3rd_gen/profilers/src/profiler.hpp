
#ifndef _PROFILER_HPP_
#define _PROFILER_HPP_

#include <assert.h>
#include "../../api/ubiprof.hpp"
#include "../../common/include/cycle.h"
#include "globals.hpp"

/**
 *  Profilers needing to run a monitor (e.g: a background thread) for bookkeeping tasks etc.
 *  need to implement this
 * */

class Monitorable {

    public:
          virtual void spawnMonitor() = 0;

};

/* Stack entry */

typedef struct InvocationData {
  ticks timestamp;
  uint16_t func_id;
  uint64_t prolog_leaf_count;
  uint64_t epilog_leaf_count;
  uint64_t leaf_count;
} InvocationData;


/*
typedef struct TLStatistics {
  uint64_t thread_local_overhead; // Profiling overhead incurred by this thread
  uint64_t thread_local_count;    // Number of samples captured by this thread
  void* func_stats;
} TLStatistics;
*/


#endif /* _PROFILER_HPP_ */
