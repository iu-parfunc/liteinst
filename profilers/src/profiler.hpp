
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

#endif /* _PROFILER_HPP_ */
