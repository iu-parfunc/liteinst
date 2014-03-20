
#ifndef PROFILER_HPP_
#define PROFILER_HPP_

#include "cycle.h"

typedef struct prof_data {
	ticks start;
	int count;
	uint64_t min;
	uint64_t max;
	uint64_t sum;
} prof_data;

void start_profile(char* method, void* fun);

void stop_profile(char* method);

void profile_all();

void turn_off_profiler();

void profiler_func();

#endif /* PROFILER_HPP_ */
