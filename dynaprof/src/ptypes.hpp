
#ifndef PTYPES_HPP_
#define PTYPES_HPP_

#include "cycle.h"
#include <string>
#include <map>
#include <stack>
#include <inttypes.h>
#include <time.h>
#include <cstddef>
#include "nbqueue.h"

using namespace std;

#define NANO_TO_MILLIS = 1000000L 

int DEACTIVATION_THRESHOLD = 10000;
int MSG_RATE_THRESHOLD = 50;

typedef struct invocation_data {
  ticks timestamp;
  long func_id;
  unsigned long long prolog_leaf_count;
  unsigned long long epilog_leaf_count;
} invocation_data;

typedef struct dyn_global_data {
  int deactivation_count;
  const char* func_name;
  uint64_t count;
  uint64_t min;
  uint64_t max;
  uint64_t sum;
  double avg;
  double var;
  uint64_t time_histogram[11];
  uint64_t rate_histogram[11];
  bool is_leaf;

  /** Accounting data **/
  ticks latest_activation_time;
  uint64_t limited_count;
  uint64_t prolog_leaf_count;
  uint64_t epilog_leaf_count;
  uint64_t count_at_last_activation;
  int lock;
  bool active;

} dyn_global_data;

typedef struct dyn_thread_data {
  uint64_t count;
  uint64_t min;
  uint64_t max;
  uint64_t sum;
  double avg;
  double var;
  uint64_t time_histogram[11];
  uint64_t rate_histogram[11];
  uint32_t deactivation_count;
  bool is_leaf;

  /** Accounting data **/
  uint64_t count_at_last_activation;
  uint32_t stack_depth;
  uint64_t limited_count;
  uint64_t ignore_count;
  uint64_t prolog_leaf_count;
  uint64_t epilog_leaf_count;
  invocation_data invocation_stack[20];

} dyn_thread_data;

class Profiler {

public :
	void (*profiler_prolog)();
	void (*profiler_epilog)();
	void start_profile(string method, void (*prolog_func)(), void (*epilog_func)());
	void start_profile(int method_id, void (*prolog_func)(), void (*epilog_func)());
	void stop_profile(string method);
	void stop_profile(int method_id);
  void register_thread_data(dyn_thread_data*);
	void profile_all(void (*prolog_func)(), void (*epilog_func)());
  // void(*get_profiler_prolog())();
  // void(*get_profiler_epilog())();
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

#endif /* PTYPES_HPP_ */
