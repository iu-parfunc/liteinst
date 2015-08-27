
#ifndef _CALIBRATE_HPP_
#define _CALIBRATE_HPP_

#include "fastinst.hpp"
#include "cycle.h"

// Do not mangle this function
extern "C" {

  inline ticks calibrationFunction() {

    ticks start, end;
    start = getticks();
    // Simulating cyg call
    __cyg_profile_func_enter((void*)(calibrationFunction), &&enter_return); // Label as value here 
    enter_return:

    // Simulating cyg call
    __cyg_profile_func_exit((void*)(calibrationFunction), &&exit_return); // Label as value here 
    exit_return:
    end = getticks();

    return (end - start);

  }
}

namespace calibrate {

  uint64_t getInstrumentationOverheadPerProbe(); 
  void emptyInstrumentation(ProbeArg func_id);
  void calibrationCallback(const ProbeMetaData* pmd);
}

#endif /* _CALIBRATE_HPP_ */
