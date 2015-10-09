
/** This file not in use **/

#ifndef PROFILER_TYPES_HPP_
#define PROFILER_TYPES_HPP_

#include "cycle.h"
#include <string>
#include <map>
#include <stack>
#include <inttypes.h>
#include <time.h>
#include <cstddef>
#include "nbqueue.h"
// #include "tbb/concurrent_queue.h"
// #include "tbb/internal/_concurrent_queue_impl.h"

using namespace std;
// using namespace tbb;

#define NANO_TO_MILLIS = 1000000L 

/*
template<class T>
class concurrent_ring {

  concurrent_bounded_queue<T> cbq;

  public:
  void push(const T& source, T& evicted) {
    while(!cbq.try_push(source)) {
       cbq.try_pop(evicted);
    }
  }

  bool try_push(const T& source) {
    return cbq.try_push(source);
  }

  void pop(T& destination) {
    cbq.pop(destination);
  }

  bool try_pop(T& destination) {
    return cbq.try_pop(destination);
  }

  bool empty() {
    return cbq.empty();
  }

  void set_capacity(size_t new_capacity) {
    cbq.set_capacity(new_capacity);
  }

  size_t capacity() {
    return cbq.capacity();
  }

};
*/

int DEACTIVATION_THRESHOLD = 10000;
int MSG_RATE_THRESHOLD = 50;

// typede<string, uint64_t> timestamps;

typedef struct invocation_data {
  ticks timestamp;
  long func_id;
  unsigned long long leaf_count;
} invocation_data;

typedef stack<invocation_data> ts_stack;

// extern stat_stack st_stack;

typedef struct func_data {
  ticks start;
  ticks last_deactivation;
  ticks last_activation;
  uint64_t last_extended_count; // Last extended count extended profiling
  ticks last_delta_checkpoint; // Monitor thread last checkpoint time for this function
  // uint64_t last_checkpoint_count; // Monitor thread last checkpoint count for this function
  uint64_t last_count;
  uint64_t last_delta; // Last message count delta for this function
  uint64_t last_checkpoint_count;
  uint64_t current_delta;
  int deactivation_count;
  int weight;
  const char* func_name;
  uint64_t count;
  uint64_t extended_count;
  uint64_t min;
  uint64_t max;
  uint64_t sum;
  double avg;
  double var;
  uint64_t time_histogram[11];
  uint64_t rate_histogram[11];
  // NBQueue* histogram;
  int lock;
  int status; // 2 - Extended 1 - Basic 0 - Disabled
  bool is_leaf;
} func_data;

typedef struct thr_func_data {
  uint32_t count;
  uint64_t min;
  uint64_t max;
  uint64_t sum;
  ticks last_sync_ts;
} thr_func_data;

typedef struct thr_local_data {
  ts_stack* ts;
  thr_func_data* func_data;
} thr_local_data;

/*
typedef func_queue {
  int* arr;
  int ptr;
}
*/

class Profiler {

public :
	void (*profiler_prolog)();
	void (*profiler_epilog)();
	void start_profile(string method, void (*prolog_func)(), void (*epilog_func)());
	void start_profile(int method_id, void (*prolog_func)(), void (*epilog_func)());
	void stop_profile(string method);
	void stop_profile(int method_id);
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

#endif /* PROFILER_TYPES_HPP_ */
