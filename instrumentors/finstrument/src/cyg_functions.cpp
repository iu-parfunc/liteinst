
#include <unistd.h>
#include <inttypes.h>
#include <stdlib.h>
#include <alloca.h>
#include <setjmp.h>
#include <assert.h> 

#include "cyg_functions.hpp"
#include "utils.hpp"

#include "finstrumentor.hpp"
#include "../../../common/include/cycle.h"
#include "bitmap.hpp"

extern uint64_t g_TicksPerNanoSec;

static bool is_prolog_initialized(uint64_t func_addr) {

  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;
  return ins->hasProbeInfo(func_addr);
}

#ifdef PROBE_HIST_ON
char* md = getenv("MONITOR_DEAC");

// Note : Calculations within this method is not thread safe
void update_overhead_histograms(TLStatistics* ts, uint64_t overhead, int type) {
  int bin;
  if (overhead > PROBE_HIST_MAX_VALUE) {
    bin = g_num_bins - 1;
  } else {
    bin = overhead / BIN_SIZE;
  }

  if (type) {
    g_prolog_timings[bin]++; 
    // Online calculation of mean and standara deviation
    // if (overhead < 1000000) { // Trying skip context switches
      g_prolog_count++;
      double delta = overhead - g_prolog_mean;
      g_prolog_mean = g_prolog_mean + delta/g_prolog_count;
      g_prolog_variance = g_prolog_variance + delta * (overhead - g_prolog_mean);
    // }
  } else {
    g_epilog_timings[bin]++;

    // Online calculation of mean and standara deviation
    // if (overhead < 1000000) { // Trying skip context switches
      g_epilog_count++;
      double delta = overhead - g_epilog_mean;
      g_epilog_mean = g_epilog_mean + delta/g_epilog_count;
      g_epilog_variance = g_epilog_variance + delta * (overhead - g_epilog_mean);
    // }

    uint64_t total_probe_overhead = ts->prolog_overhead + overhead;
    if (total_probe_overhead > PROBE_HIST_MAX_VALUE) {
      bin = g_num_bins - 1;
    } else {
      bin = total_probe_overhead / BIN_SIZE;
    }
    g_probe_timings[bin]++;

    // Online calculation of mean and standara deviation
    // if (total_probe_overhead < 1000000) { // Trying skip context switches
      g_total_probe_count++;
      delta = total_probe_overhead - g_probe_mean;
      g_probe_mean = g_probe_mean + delta/g_total_probe_count;
      g_probe_variance = g_probe_variance + delta * (total_probe_overhead - g_probe_mean);
    // }
  }
}
#endif

#ifdef PROBE_TRUE_EMPTY_ON

void update_empty_overheads(uint64_t overhead, int type) {
  int bin;
  if (overhead > PROBE_HIST_MAX_VALUE) {
    bin = g_num_bins - 1;
  } else {
    bin = overhead / BIN_SIZE;
  }

  if (type) {
    g_prolog_timings[bin]++; 
  } else {
    g_epilog_timings[bin]++;
  }
}

#endif

// BJS: START cyg func refactor attempt 

#define IS_FUNC_ID(x) ((x) < 0x400200) 

/* -----------------------------------------------------------------
   ENTER HELPERS 
   ----------------------------------------------------------------- */ 
static inline void process_func_by_id(uint64_t function, ticks start) {
  TLStatistics* tstats;

  assert(IS_FUNC_ID(function));
 	 
  // fprintf(stderr, "\n[cyg_enter] Low function address  : %lu\n", ((uint64_t)func));
  // ts = prologFunction((uint16_t)func);
  tstats = prologFunction(function);

  assert(tstats != NULL); 
  
#ifdef PROBE_CPU_TIME
  struct timespec ts1;
  clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts1);
  ticks end= (ticks)((ts1.tv_sec * 1000000000LL + ts1.tv_nsec) * g_TicksPerNanoSec);
#else 
  ticks end = getticks();
#endif
  
  uint64_t prolog_overhead = (end - start);
  tstats->thread_local_overhead += prolog_overhead;
  tstats->prolog_overhead = prolog_overhead;
  
#ifdef PROBE_HIST_ON
  update_overhead_histograms(tstats, prolog_overhead, PROLOG); 
#endif
  
  return;
}


/* -----------------------------------------------------------------
   ENTER FUNCTIONS 
   ----------------------------------------------------------------- */
#ifdef PROBE_TRUE_EMPTY_ON 
void __cyg_profile_func_enter(void* func, void* caller) {
  #ifdef PROBE_CPU_TIME
    struct timespec ts0;
    struct timespec ts1;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts0);
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts1);

    ticks start = (ticks)((ts0.tv_sec * 1000000000LL + ts0.tv_nsec) * g_TicksPerNanoSec);
    ticks end = (ticks)((ts1.tv_sec * 1000000000LL + ts1.tv_nsec) * g_TicksPerNanoSec);
  #else
    ticks start = getticks();
    ticks end = getticks();
  #endif

  update_empty_overheads(end - start, PROLOG); 
  return;
}
#else
void __cyg_profile_func_enter(void* func, void* caller) {
  
  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;
  int64_t flag = (int64_t) caller;
  uint64_t function = (int64_t) func; // this is either an address or an ID
  // BJS: moved some decls to top. I think the compiler will put 
  //  these here anywhere. 
  
#ifdef PROBE_CPU_TIME
  struct timespec ts0;
  clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts0);
  ticks start = (ticks)((ts0.tv_sec * 1000000000LL + ts0.tv_nsec) * g_TicksPerNanoSec);
#else
  ticks start = getticks();
#endif

  if (!g_ubiprof_initialized) { 
    // calibrate_cache_effects is expected to take place before initialization (during) 
    // At least if I read the code correctly (initializer.cpp and minimal_adaptive_profiler.cpp)
    // g_ubiprof_initialized is set = true after the call to calibrate_cache_effects. 

    if (flag == -1) {
      // I'm  not sure what the expectation is on getFunctionCount before initialization!! 
      // This code seems to expect that intialization is somewhat done. 
      int64_t tmp_id = ins->getFunctionCount();
      function = tmp_id - 1;
      
      // At this point we should be sure that function is an ID 
      
      process_func_by_id(function,start);
      // At this point we are done with the -1 flag and can EXIT this function.
      return; 
    }
    // not initialized and not the special case... exit 
    else return; 
  }

  // NOW AT THIS POINT WE KNOW THAT UBIPROF IS INITIALIZED 
  assert(g_ubiprof_initialized == true); 
  // I WANT THE ASSERT BELOW TO PASS 
  assert(flag >= 0); 

  // This is the branch probably taken most often. 
  if (IS_FUNC_ID(function)) { 
    process_func_by_id(function,start); 
    
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
  assert(((uint64_t)addr < function) == false);   
  assert((func_id > 0) == true); 
	 
  // NOW PATCH ARGUMENTS (Set up for next run to enter the efficient branch) 

  FinsProbeInfo* probe_info = ins->getProbeInfo((uint64_t) func, (uint8_t*) addr);
  if (probe_info != NULL && probe_info->unpatched) {
    ; // Escape to just executing prolog function
  } else {
    PatchResult* res  = patch_first_parameter(addr, (uint64_t*) func, func_id);

    if (!res->success) {
      if (res->conflict) {
        fprintf(stderr, "[Finstrumentor] Detected straddler conflict at %p ..\n", (void*)addr);
      }
        
      ins->addProbeInfo((uint64_t)func, (uint8_t*)addr, true);
      // fprintf(stderr, "[Finstrumentor] Adding straddler conflict at %p  function %p with probe info unpatched at %p ..\n", func, (void*)addr, (void*)probe_info);
      probe_info = ins->getProbeInfo((uint64_t) func, (uint8_t*) addr);
      // fprintf(stderr, "[Finstrumentor] After adding conflict at %p function %p : %p\n", (void*)addr, func, (void*)probe_info->unpatched);

      // Mark this as a function to escape patching
      set_index(g_straddlers_bitmap, func_id);

    } else {
      ins->addProbeInfo((uint64_t)func, (uint8_t*)addr, false);
    }

    delete res;
  }

  // Finish off by using the normal process_func_by_id 
  process_func_by_id(func_id,start); 
}
#endif


/* 
void __cyg_profile_func_enter(void* func, void* caller) {

#ifdef PROBE_TRUE_EMPTY_ON
  #ifdef PROBE_CPU_TIME
    struct timespec ts0;
    struct timespec ts1;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts0);
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts1);

    ticks start = (ticks)((ts0.tv_sec * 1000000000LL + ts0.tv_nsec) * g_TicksPerNanoSec);
    ticks end = (ticks)((ts1.tv_sec * 1000000000LL + ts1.tv_nsec) * g_TicksPerNanoSec);
  #else
    ticks start = getticks();
    ticks end = getticks();
  #endif

  update_empty_overheads(end - start, PROLOG); 
  return;
#else 
  #ifdef PROBE_CPU_TIME
    struct timespec ts0;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts0);
    ticks start = (ticks)((ts0.tv_sec * 1000000000LL + ts0.tv_nsec) * g_TicksPerNanoSec);
  #else
    ticks start = getticks();
  #endif


  // Experimental parameter patching code
  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;
  // uint64_t funcAddr = *((uint64_t*) func);

  // If the Ubiprof library has not yet been properly initialized return.
  // But caller parameter being -1 signals a special explicit invocation
  // of the instrumentation which is done for calibration purposes at 
  // the library init time. If that's the case we atually want to continue
  // executing.
  int64_t flag = (int64_t) caller;
  if (!g_ubiprof_initialized && flag != -1) {
    // fprintf(stderr, "Returning from func addr : %p\n", func);
    return;
  }

  // Explicitly set the function id for calibrate_cache_effects
  if (flag == -1) {
    func = (void*) (ins->getFunctionCount() - 1);
  }

  TLStatistics* ts;
  if ((uint64_t) func < 0x400200) {
    // fprintf(stderr, "\n[cyg_enter] Low function address  : %lu\n", ((uint64_t)func));
    // ts = prologFunction((uint16_t)func);
    ts = prologFunction((uint64_t)func);

  #ifdef PROBE_CPU_TIME
    struct timespec ts1;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts1);
    ticks end= (ticks)((ts1.tv_sec * 1000000000LL + ts1.tv_nsec) * g_TicksPerNanoSec);
  #else 
    ticks end = getticks();
  #endif

    uint64_t prolog_overhead = (end - start);
    ts->thread_local_overhead += prolog_overhead;
    ts->prolog_overhead = prolog_overhead;

#ifdef PROBE_HIST_ON
    update_overhead_histograms(ts, prolog_overhead, PROLOG); 
#endif

    return;
  }

  uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
  uint16_t func_id = ins->getFunctionId((uint64_t)func);

  if (addr < func) {
    // fprintf(stderr, "Function start is great than the cyg_enter return address.. Function address: %p Call address : %p \n", func, addr);
  #ifdef PROBE_CPU_TIME
    struct timespec ts1;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts1);
    ticks end= (ticks)((ts1.tv_sec * 1000000000LL + ts1.tv_nsec) * g_TicksPerNanoSec);
  #else 
    ticks end = getticks();
  #endif

    uint64_t prolog_overhead = (end - start);
    if (ts != NULL) {
      ts->thread_local_overhead += prolog_overhead;
    } else { 
      fprintf(stderr,"cyg_enter: ts is not initialized, exiting.\n");
      exit(EXIT_FAILURE);
    }

  #ifdef PROBE_HIST_ON
    update_overhead_histograms(ts, prolog_overhead, PROLOG); 
  #endif

    return;
  }

  // If the function id mappings are not properly initialized fail gracefully
  if (!func_id && flag != -1) { 
    fprintf(stderr, "Returning from func addr : %p\n", func);
    return;
  }

  FinsProbeInfo* probe_info = ins->getProbeInfo((uint64_t) func, (uint8_t*) addr);
  if (probe_info != NULL && probe_info->unpatched) {
    ; // Escape to just executing prolog function
  } else {
    PatchResult* res  = patch_first_parameter(addr, (uint64_t*) func, func_id);

    if (!res->success) {
      if (res->conflict) {
        fprintf(stderr, "[Finstrumentor] Detected straddler conflict at %p ..\n", (void*)addr);
      }
        
      ins->addProbeInfo((uint64_t)func, (uint8_t*)addr, true);
      // fprintf(stderr, "[Finstrumentor] Adding straddler conflict at %p  function %p with probe info unpatched at %p ..\n", func, (void*)addr, (void*)probe_info);
      probe_info = ins->getProbeInfo((uint64_t) func, (uint8_t*) addr);
      // fprintf(stderr, "[Finstrumentor] After adding conflict at %p function %p : %p\n", (void*)addr, func, (void*)probe_info->unpatched);

      // Mark this as a function to escape patching
      set_index(g_straddlers_bitmap, func_id);

    } else {
      ins->addProbeInfo((uint64_t)func, (uint8_t*)addr, false);
    }

    delete res;
  }

  ts = prologFunction(func_id);
  ticks end = getticks();
  ts->thread_local_overhead += (end - start);
  uint64_t prolog_overhead = (end - start);
  ts->prolog_overhead = prolog_overhead;

#ifdef PROBE_HIST_ON
    update_overhead_histograms(ts, prolog_overhead, PROLOG); 
#endif

#endif
}
*/ 


void __cyg_profile_func_exit(void* func, void* caller) {

#ifdef PROBE_TRUE_EMPTY_ON
  #ifdef PROBE_CPU_TIME
    struct timespec ts0;
    struct timespec ts1;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts0);
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts1);

    ticks start = (ticks)((ts0.tv_sec * 1000000000LL + ts0.tv_nsec) * g_TicksPerNanoSec);
    ticks end = (ticks)((ts1.tv_sec * 1000000000LL + ts1.tv_nsec) * g_TicksPerNanoSec);
  #else
    ticks start = getticks();
    ticks end = getticks();
  #endif

  update_empty_overheads(end - start, EPILOG); 
  return;
#else 
  #ifdef PROBE_CPU_TIME
    struct timespec ts0;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts0);
    ticks start = (ticks)((ts0.tv_sec * 1000000000LL + ts0.tv_nsec) * g_TicksPerNanoSec);
  #else
    ticks start = getticks();
  #endif

  Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;

  // If the Ubiprof library has not yet been properly initialized return.
  // But caller parameter being -1 signals a special explicit invocation
  // of the instrumentation which is done for calibration purposes at 
  // the library init time. If that's the case we atually want to continue
  // executing.
  int64_t flag = (int64_t) caller;
  if (!g_ubiprof_initialized && flag != -1) {
    // fprintf(stderr, "Returning from func addr : %p\n", func);
    return;
  }

  // Explicitly set the function id for calibrate_cache_effects
  if (flag == -1) {
    func = (void*) (ins->getFunctionCount() - 1);
  }

  TLStatistics* ts;
  if ((uint64_t) func < 0x400200) {
    // fprintf(stderr, "\n[cyg_exit] Low function address  : %lu\n", ((uint64_t)func));
    // ts = epilogFunction((uint16_t)func);
    ts = epilogFunction((uint64_t)func);

  #ifdef PROBE_CPU_TIME
    struct timespec ts1;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts1);
    ticks end= (ticks)((ts1.tv_sec * 1000000000LL + ts1.tv_nsec) * g_TicksPerNanoSec);
  #else 
    ticks end = getticks();
  #endif

    uint64_t epilog_overhead = (end - start);
    ts->thread_local_overhead += epilog_overhead;

  #ifdef PROBE_HIST_ON
    update_overhead_histograms(ts, epilog_overhead, EPILOG); 
    /*
    if (!ts->deactivated) {
      if (md == NULL) {
        // fprintf(stderr, "Updating REGULAR flow\n");
        update_overhead_histograms(ts, epilog_overhead, EPILOG); 
      }
    } else {
      if (md != NULL) {
        fprintf(stderr, "Updating DEACTIVATION flow\n");
        update_overhead_histograms(ts, epilog_overhead, EPILOG); 
      }
      ts->deactivated = false;
    }
    */
  #endif

    return;
  }

  uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));
  uint16_t func_id = ins->getFunctionId((uint64_t)func);
  // uint16_t func_id = get_func_id((uint64_t)func);

  if (addr < func) {
    // fprintf(stderr, "Function start is great than the cyg_exit return address.. Function address: %p Call address : %p \n", func, addr);
  #ifdef PROBE_CPU_TIME
    struct timespec ts1;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts1);
    ticks end= (ticks)((ts1.tv_sec * 1000000000LL + ts1.tv_nsec) * g_TicksPerNanoSec);
  #else 
    ticks end = getticks();
  #endif

    uint64_t epilog_overhead = (end - start);
    if (ts != NULL){
      ts->thread_local_overhead += epilog_overhead;
    } else {
      fprintf(stderr,"cyg_exit: ts is not initialized, exiting.\n");
      exit(EXIT_FAILURE); 
    }

  #ifdef PROBE_HIST_ON
    update_overhead_histograms(ts, epilog_overhead, EPILOG); 
  #endif

    return;
  }

  // If the function id mappings are not properly initialized fail gracefully
  if (!func_id && flag != -1) { 
    fprintf(stderr, "Returning from func addr : %p\n", func);
    return;
  }

  // For some reason (mostly compiler scrweing things up at prolog) the prolog has not been properly initialized. 
  if (!is_prolog_initialized((uint64_t)func)) {
    return;
  }

  FinsProbeInfo* probe_info = ins->getProbeInfo((uint64_t) func, (uint8_t*) addr);
  if (probe_info != NULL && probe_info->unpatched) {
    ; // Escape to just executing epilog function
  } else {
    PatchResult* res  = patch_first_parameter(addr, (uint64_t*) func, func_id);

    if (!res->success) {
      if (res->conflict) {
        fprintf(stderr, "[Finstrumentor] Detected straddler conflict at %p ..\n", (void*)addr);
      }
        
      ins->addProbeInfo((uint64_t)func, (uint8_t*)addr, true);
      fprintf(stderr, "[Finstrumentor] Patching failed at %p function %p ..\n", (void*)addr, func);
      probe_info = ins->getProbeInfo((uint64_t) func, (uint8_t*) addr);
      // fprintf(stderr, "[Finstrumentor] After adding conflict at %p function %p : %d\n", (void *)addr, func, probe_info->unpatched);
      
      // Mark this as a function to escape patching
      set_index(g_straddlers_bitmap, func_id);
    } else {
      ins->addProbeInfo((uint64_t)func, (uint8_t*)addr, false);
    }

    delete res;
  }

  ts = epilogFunction(func_id);
  ticks end = getticks();
  ticks epilog_overhead = (end - start);
  ts->thread_local_overhead += epilog_overhead;

#ifdef PROBE_HIST_ON
    update_overhead_histograms(ts, epilog_overhead, EPILOG); 
#endif

#endif

}
