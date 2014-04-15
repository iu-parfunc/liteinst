
#ifndef PROFILER_HPP_
#define PROFILER_HPP_

#include "cycle.h"
#include <string>
#include <map>

using namespace std;

class Profiler {

protected:
	void (*fun)();

public :
	void start_profile(string method, void (*fun)());
	void stop_profile(string method);
	void profile_all(void (*fun)());
	void turn_off_profiler();
	virtual void set_profiler_function() = 0;
	virtual ~Profiler() {}
};

class Basic_Profiler : public Profiler {
public :
	Basic_Profiler() {set_profiler_function();}
	void set_profiler_function();
	virtual ~Basic_Profiler() {}
};

void basic_profiler_func();

/*typedef struct prof_data {
	ticks start;
	int count;
	uint64_t min;
	uint64_t max;
	uint64_t sum;
} prof_data;

typedef std::map<std::string, prof_data*> function_stats;

typedef std::map<std::string, prof_data*> global_stats;*/

#endif /* PROFILER_HPP_ */
