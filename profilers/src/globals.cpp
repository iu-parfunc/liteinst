
#include <stdlib.h>
#include <inttypes.h> 

#include "globals.hpp"

/* Globals for sampling and adaptive profilers */
uint64_t sp_initial_sample_size;
uint64_t sp_sample_size;
double sp_epoch_period;
double sp_target_overhead;
uint64_t g_total_overhead = 0; // Overhead incurred due to profiling
uint64_t g_total_process_time = 0; // Total process time until last epoch sample
uint64_t g_last_epoch_random = 0; // Random added to last epoch period
uint64_t g_TicksPerNanoSec = 0; // Calibrated ticks per nano second
uint64_t g_call_overhead = 0; // Call overhead calibrated value
uint16_t g_strategy = EPOCH_CONTROL; // Overhead control strategy to use
uint64_t g_thread_lock = 0;
uint64_t g_shutting_down_flag = 0;
