
#ifndef _GLOBALS_HPP_
#define _GLOBALS_HPP_

#include <inttypes.h>
#include <list>
#include <string>

//TODO: Make this an enum
#define SLOW_RAMP_UP 0
#define PROPOTIONAL 1
#define EPOCH_CONTROL 2

// Application signals to handle probe monitor thread shut down safely
#define NO_TERMINATE_REQUESTS 0
#define TERMINATE_REQUESTED 1
#define TERMINATED 2

extern int g_profiler_type;

// Sampling and Adaptive profiler specific
// Temporary exports to make overhead statistics visible to finalizer
extern pthread_t g_monitor_thread;
extern uint64_t g_probe_overheads;
extern uint64_t g_probe_count;

extern uint64_t sp_initial_sample_size;
extern uint64_t sp_sample_size;
extern double sp_epoch_period;
extern double sp_target_overhead;
extern uint64_t g_total_overhead; // Overhead incurred due to profiling
extern uint64_t g_total_process_time; // Total process time until last epoch sample
extern uint64_t g_last_epoch_random; // Random added to last epoch period
extern uint64_t g_TicksPerNanoSec; // Calibrated ticks per nano second
extern uint64_t g_call_overhead; // Call overhead calibrated value
extern uint16_t g_strategy; // Overhead control strategy to use
extern uint64_t g_thread_lock;
extern uint64_t g_shutting_down_flag; // Flag to indicate the application is terminating. 
                                      // Used to alert probe monitor thread to terminate safely

#endif /* _GLOBALS_HPP_ */
