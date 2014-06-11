
#ifndef PROFILER_TYPES_HPP_
#define PROFILER_TYPES_HPP_

#include "cycle.h"
#include <string>
#include <map>
#include <stack>
#include <inttypes.h>
#include "nbqueue.h"

using namespace std;

int DEACTIVATION_THRESHOLD = 1000;

typedef map<string, uint64_t> timestamps;

typedef stack<ticks> ts_stack;

// extern stat_stack st_stack;

typedef struct func_data {
  ticks start;
  ticks last_deactivation;
  const char* func_name;
  int deactivation_count;
  bool newly_reactivated;
  uint64_t count;
  uint64_t min;
  uint64_t max;
  uint64_t sum;
  uint64_t avg;
  int lock;
} func_data;

class Profiler {

protected:
	void (*profiler_prolog)();
	void (*profiler_epilog)();

public :
	void start_profile(string method, void (*fun)());
	void start_profile(int method_id, void (*fun)());
	void stop_profile(string method);
	void stop_profile(int method_id);
	void profile_all(void (*fun)());
	void turn_off_profiler();
	virtual void set_profiler_function() = 0;
	virtual ~Profiler() {}
};

class Basic_Profiler : public Profiler {

public :
	Basic_Profiler() {initialize();}
  void initialize();
	void set_profiler_function();
	virtual ~Basic_Profiler() {}

};

// void basic_profiler_func();

#endif /* PROFILER_TYPES_HPP_ */
