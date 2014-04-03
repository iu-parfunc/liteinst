
#ifndef PROFILER_HPP_
#define PROFILER_HPP_

#include "cycle.h"
#include <string>
#include <map>

using namespace std;

typedef struct prof_data {
	ticks start;
	int count;
	uint64_t min;
	uint64_t max;
	uint64_t sum;
} prof_data;

typedef std::map<std::string, prof_data*> function_stats;

typedef std::map<std::string, prof_data*> global_stats;

void start_profile(string method, void (*fun)());

void stop_profile(string method);

void profile_all();

void turn_off_profiler();

void profiler_func();

#endif /* PROFILER_HPP_ */
