
#ifndef _CALIBRATE_HPP_
#define _CALIBRATE_HPP_

#include "fastinst.hpp"
#include "cycle.h"

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

  uint64_t getInstrumentationOverheadPerFunction(); 
  void emptyInstrumentation(ProbeArg func_id);
  void calibrationCallback(const ProbeMetaData* pmd);

  /*
  void __fake_cyg_enter_function() {
    FinstrumentProbeProvider* ins = (Finstrumentor*) ProbeProvider.PROBE_PROVIDER;
    int64_t flag = (int64_t) call_site_addr;
    uint64_t function = (uint64_t) func_addr; // this is either an address or an ID

    // If not initialized, just exit. 
    if (!g_ubiprof_initialized) { 
      return; 
    }

#ifdef PROBE_CPU_TIME
    struct timespec ts0;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts0);
    ticks start = (ticks)((ts0.tv_sec * 1000000000LL + ts0.tv_nsec) * g_TicksPerNanoSec);
#else
    ticks start = getticks();
#endif

    // NOW AT THIS POINT WE KNOW THAT UBIPROF IS INITIALIZED 
    assert(g_ubiprof_initialized == true); 
    // I WANT THE ASSERT BELOW TO PASS 
    assert(flag >= 0); 

    // This is the branch probably taken most often. 
    if (IS_FUNC_ID(function)) { 
      process_func_by_id_enter(function,start); 

      // At this point we should be done with a completely initialized function 
      // and can EXIT
      return; 
    }

    // NOW DEAL WITH FUNCTIONS THAT YET DO NOT HAVE AN ID: 
    // This is part of "system-start-up" (one-time initialization, first time run)
    assert(IS_FUNC_ID(function) == false); 

    uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
    // This creates a new function ID ? 
    uint16_t func_id = ins->getFunctionId(function);

    // Replaced branches that never seemed to happen with asserts. 
    //assert(((uint64_t)addr < function) == false);   

    if ((uint64_t)addr < function) { 
      //fprintf(stderr, "ENTER_ERR: What does this mean!? %llx < %llx\n",(uint64_t)addr,function); 
#ifdef _GNU_SOURCE
      Dl_info d;
      (void)dladdr(addr, &d);
      fprintf(stderr,"cyg_enter was called from function %s\n",
          d.dli_sname); 
      fprintf(stderr," * Object file: %s\n", d.dli_fname);
      fprintf(stderr," * Loaded into (base) 0x%p\n", d.dli_fbase);
      fprintf(stderr," * Addr of caller (symbol)  0x%p\n", d.dli_saddr);
      fprintf(stderr,"Possibly caused by function inlining\n");
#endif 
      fprintf(stderr,"Compile with -fno-inline and at most -O2\n"); 
      exit(EXIT_FAILURE); 
    }

    assert((func_id > 0) == true); 

    // NOW PATCH ARGUMENTS (Set up for next run to enter the efficient branch) 

    FinsProbeInfo* probe_info = ins->getProbeInfo((uint64_t) func, (uint8_t*) addr);

    //BJS: I think this does the same.
    if (probe_info == NULL) {

      // Enable write permission to call site
      modify_page_permissions((uint8_t*) addr - 5);
      patch_first_parameter(probe_info, addr, (uint64_t*) func, func_id);
    }

    process_func_by_id_enter(func_id,start); 
    }

    }
  }
*/

}

#endif /* _CALIBRATE_HPP_ */
