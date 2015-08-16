/**
 * @file cyg_functions.cpp
 * @author Buddhika Chamith, Bo Joel Svensson, Ryan Newton 
 * @brief cyg_profile_function_enter/exit and related definitions.
 *
 */

#include <unistd.h>
#include <inttypes.h>
#include <stdlib.h>
#include <alloca.h>
#include <setjmp.h>
#include <assert.h> 

#include <string>

#include "finstrument_probe_provider.hpp"
#include "cyg_functions.hpp"

#include "../../common/include/cycle.h"

#ifdef _GNU_SOURCE
#include <dlfcn.h>
#endif

using namespace std;

extern uint64_t g_TicksPerNanoSec;
extern bool g_ubiprof_initialized;

/**
 * @brief check if function at addr func_addr has an initialized prolog. 
 * @param func_addr address of function 
 * @details This function performs a lookup in a map #probe_map via the 
 * call Finstrumentor::hasProbeInfo().
 */
static bool is_prolog_initialized(uint64_t func_addr) {

  return true;

  // Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;
  // return ins->hasProbeInfo(func_addr);
}

#ifdef PROBE_HIST_ON
char* md = getenv("MONITOR_DEAC");


/** 
 * @brief  
 * @param ts  
 * @param overhead
 * @param type 
 * @details BUDDHIKA DOCUMENT THIS 
 * Calculations within this method is not thread safe
 */
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

/** 
 * @brief  
 * @param overhead
 * @param type 
 * @details BUDDHIKA DOCUMENT THIS 
 */
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

/** 
 * @brief check if x should be considered a function id.
 * @param x a small integer identifying a function or an address of a function 
 * @details 
 */
#define IS_PROBE_ID(x) ((x) < 0x400200)

/* -----------------------------------------------------------------
   ENTER/Exit HELPERS 
   ----------------------------------------------------------------- */ 
/** 
 * @brief helper function that is called by __cyg_profile_func_enter() when 
   when a function id has been established. 
 * @param function a function id (that really should fit in 16 bits) 
 * @param start a timestamp (in ticks) acquired upon entry into __cyg_profile_func_enter().
 * @details This function calls the specified prolog function and then updates the 
 * thread-local datastructure containing prolog overhead. 
 */
static inline void process_func_enter(InstrumentationFunc func, ProbeArg probe_arg) {

  func(probe_arg);
  
#ifdef PROBE_CPU_TIME
  struct timespec ts1;
  clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts1);
  ticks end= (ticks)((ts1.tv_sec * 1000000000LL + ts1.tv_nsec) * g_TicksPerNanoSec);
#else 
  ticks end = getticks();
#endif
  
  // uint64_t prolog_overhead = (end - start);
  
#ifdef PROBE_HIST_ON
  update_overhead_histograms(tstats, prolog_overhead, PROLOG); 
#endif
  
  return;
}

/** 
 * @brief helper function that is called by __cyg_profile_func_exit() when 
   when a function id has been established. 
 * @param function a function id (that really should fit in 16 bits) 
 * @param start a timestamp (in ticks) acquired upon entry into __cyg_profile_func_exit().
 * @details This function calls the specified epilog function and then updates the 
 * thread-local datastructure containing epilog overhead. 
 */
static inline void process_func_exit(InstrumentationFunc func, ProbeArg probe_arg) {

  func(probe_arg);

#ifdef PROBE_CPU_TIME
  struct timespec ts1;
  clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts1);
  ticks end= (ticks)((ts1.tv_sec * 1000000000LL + ts1.tv_nsec) * g_TicksPerNanoSec);
#else 
  ticks end = getticks();
#endif

  // uint64_t epilog_overhead = (end - start);
  //#ifndef NDEBUG 
  //fprintf(stderr,"Added %ld to thread_local_overhead\n", epilog_overhead);
  //#endif 
  
#ifdef PROBE_HIST_ON
  update_overhead_histograms(ts, epilog_overhead, EPILOG); 
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
void __cyg_profile_func_enter(void* func_addr, void* call_site_addr) {
  
  FinstrumentProbeProvider* ins = (FinstrumentProbeProvider*) PROBE_PROVIDER;
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
  if (IS_PROBE_ID(function)) { 
    ProbeMetaData* pmd = ins->getProbeMetaData(function);
    process_func_enter(pmd->instrumentation_func.load(), pmd->probe_arg); 
    
    // At this point we should be done with a completely initialized function 
    // and can EXIT
    return; 
  }
  
  // NOW DEAL WITH FUNCTIONS THAT YET DO NOT HAVE AN ID: 
  // This is part of "system-start-up" (one-time initialization, first time run)
  assert(IS_PROBE_ID(function) == false); 

  uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(
      __builtin_return_address(0));

  ProbeMetaData* pmd = ins->getNewProbeMetaDataContainer((Address) addr - 5);

  // Some other thread is executing initialization profotocl for this probe 
  // site already and this thread lost it. Just return without trying to wait.
  // We may drop a few samples. But it should be a good tradeoff in terms of 
  // performance.
  if (pmd == NULL) {
    return;
  }

  pmd->func_name = ins->getFunctionName((Address) func_addr);

  string str;
  str.append(pmd->func_name).append("$").append("enter");
  pmd->probe_name = str;

  pmd->probe_addr = (uint8_t*) addr - 5;
  pmd->type = ProbeType::FINSTRUMENT;
  pmd->state = ProbeState::UNINITIALIZED; //??
  pmd->probe_context = ProbeContext::ENTRY;

  ins->registerProbe(pmd);

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

  // NOW PATCH ARGUMENTS (Set up for next run to enter the efficient branch) 
  // TBD with new libcodeswap library 
  // Finish off by using the normal process_func_by_id 
  process_func_enter(pmd->instrumentation_func, pmd->probe_arg); 
}
#endif

/* -----------------------------------------------------------------
   FAKE CYG ENTER specifically for calibrate_cache_effects
   ----------------------------------------------------------------- */ 
/** 
   * @brief A fake variant of cyg_enter specifically for calibrate_cache_effects.
   * @param this_fn   BUDDHIKA DOCUMENT THIS
   * @param call_site BUDDHIKA DOCUMENT THIS
   * @details Tries to emulate what a real cyg_enter function does in 
   * order to estimate its impact on cache.
   */
void fake_cyg_profile_func_enter(void* func, void* caller) {
  
  //Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;
  //int64_t flag = (int64_t) caller;
  uint64_t function = (uint64_t) func; // this is either an address or an ID
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
    
    // process_func_by_id_enter(function,start);
    return; 
  } else { 
    fprintf(stderr,"fake_cyg_profile_func_enter is called after initialization!\n");
  }
}

/* ----------------------------------------------------------------- 
   PROFILE EXIT FUNC 
   ----------------------------------------------------------------- */ 

#ifdef PROBE_TRUE_EMPTY_ON
void __cyg_profile_func_exit(void* func, void* caller) {
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
}

#else
/** 
   * @brief Implements cyg_exit function hook. 
   * @param func_addr Address of the calling function. 
   * @param call_site_addr Address of the next instruction after call to 
   * cyg_exit. 
   */ 
void __cyg_profile_func_exit(void* func_addr, void* call_site_addr) {

  #ifdef PROBE_CPU_TIME
    struct timespec ts0;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts0);
    ticks start = (ticks)((ts0.tv_sec * 1000000000LL + ts0.tv_nsec) * g_TicksPerNanoSec);
  #else
    ticks start = getticks();
  #endif

  FinstrumentProbeProvider* ins = (FinstrumentProbeProvider*) PROBE_PROVIDER;
  int64_t flag = (int64_t) call_site_addr;
  uint64_t function = (uint64_t) func_addr; // this is either an address or an ID

  if (!g_ubiprof_initialized) {
    return;
  }

  if (IS_PROBE_ID(function)) { 
    ProbeMetaData* pmd = ins->getProbeMetaData(function);
    process_func_exit(pmd->instrumentation_func.load(), pmd->probe_arg); 
    return; 
  }

  
  uint64_t* addr = (uint64_t*)__builtin_extract_return_addr(__builtin_return_address(0));

  ProbeMetaData* pmd = ins->getNewProbeMetaDataContainer((Address) addr - 5);

  // Some other thread is executing initialization profotocl for this probe
  // site already and this thread lost it. Just return without trying to wait.
  // We may drop a few samples. But it should be a good tradeoff in terms of
  // performance.
  if (pmd == NULL) {
    return;
  }

  pmd->func_name = ins->getFunctionName((Address) func_addr);

  string str;
  str.append(pmd->func_name).append("$").append("enter");
  pmd->probe_name = str;

  pmd->probe_addr = (uint8_t*) addr - 5;
  pmd->type = ProbeType::FINSTRUMENT;
  pmd->state = ProbeState::UNINITIALIZED; //??
  pmd->probe_context = ProbeContext::EXIT;

  ins->registerProbe(pmd); 
  
  if ((uint64_t)addr < function) { 
    //fprintf(stderr, "ENTER_ERR: What does this mean!? %llx < %llx\n",(uint64_t)addr,function); 
#ifdef _GNU_SOURCE
    Dl_info d;
    (void)dladdr(addr, &d);
    fprintf(stderr,"cyg_exit was called from function %s\n",
	    d.dli_sname); 
    fprintf(stderr," * Object file: %s\n", d.dli_fname);
    fprintf(stderr," * Loaded into (base) 0x%p\n", d.dli_fbase);
    fprintf(stderr," * Addr of caller (symbol)  0x%p\n", d.dli_saddr);
    fprintf(stderr,"Possibly caused by function inlining\n");
#endif 
    fprintf(stderr,"Compile with -fno-inline and at most -O2\n"); 
    exit(EXIT_FAILURE); 
  }

  // For some reason (mostly compiler scrweing things up at prolog) the prolog has not been properly initialized. 
  
  // this error actually happens.. what does it mean ? 
  // assert(is_prolog_initialized(function) == true);
  /*
  if ((!is_prolog_initialized((uint64_t)func))) { 
    return;
  }
  */
 
  // NOW PATCH ARGUMENTS (Set up for next run to enter the efficient branch)
  // TBD with new libcodeswap library
  // Finish off by using the normal process_func_by_id 
  process_func_exit(pmd->instrumentation_func, pmd->probe_arg); 

}
#endif

void fake_cyg_profile_func_exit(void* func, void* caller) {
  uint64_t function = (uint64_t)func;
  
  #ifdef PROBE_CPU_TIME
    struct timespec ts0;
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts0);
    ticks start = (ticks)((ts0.tv_sec * 1000000000LL + ts0.tv_nsec) * g_TicksPerNanoSec);
  #else
    ticks start = getticks();
  #endif

    //Finstrumentor* ins = (Finstrumentor*) INSTRUMENTOR_INSTANCE;

  // If the Ubiprof library has not yet been properly initialized return.
  // But caller parameter being -1 signals a special explicit invocation
  // of the instrumentation which is done for calibration purposes at 
  // the library init time. If that's the case we atually want to continue
  // executing.
  //int64_t flag = (int64_t) caller;

  if (!g_ubiprof_initialized) {
  
    // process_func_by_id_exit(function, start); 
    return; 
  } else {
    fprintf(stderr,"fake_cyg_profile_func_enter is called after initialization!\n");
  }

}
