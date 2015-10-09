
#ifdef OVERHEAD_TIME_SERIES

#include "overhead_series.hpp"

using namespace std;

uint64_t g_time_step = 0; // Current time step in epoch time series
uint64_t g_skipped_epochs = 0; // Number skpped epochs

// List to hold the time series data
list<string>* overhead_time_series;

void record_overhead_histogram(double overhead, int64_t sample_or_epoch_size) {

    g_time_step++;

    char buf[50];
    int r = snprintf(buf, 50, "%lu,%.2lf,%ld\n", g_time_step, overhead, sample_or_epoch_size);
    if (r > 0) {
      string s = buf;
      overhead_time_series->push_back(s);
    }

}
#endif

