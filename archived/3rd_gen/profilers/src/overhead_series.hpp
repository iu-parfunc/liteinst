
#ifndef _OVERHEAD_SERIES_HPP_
#define _OVERHEAD_SERIES_HPP_

#ifdef OVERHEAD_TIME_SERIES

#include <inttypes.h>
#include <list>
#include <string>

extern void record_overhead_histogram(double overhead, int64_t sample_or_epoch_size);

extern uint64_t g_time_step; // Current time step in epoch time series
extern uint64_t g_skipped_epochs; // Number skpped epochs

// List to hold the time series data
extern std::list<std::string>* overhead_time_series;
extern void record_overhead_histogram();

#endif

#endif /* _OVERHEAD_SERIES_HPP_ */
