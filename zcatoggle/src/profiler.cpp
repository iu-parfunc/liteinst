
#include "zca-toggle.hpp"
#include "elf-provider.h"
#include "profiler.hpp"
#include <stdio.h>
#include <pthread.h>
#include <time.h>
#include <stdlib.h>
#include <unistd.h>
#include <string>
#include <vector>
#include <string.h>
#include "cycle.h"

using namespace std;

global_stats statistics;
volatile int spin_lock = 0;

void func2();

void start_profile(string method, void (*fun)() ) {

  string probe_start_annotation = method + "_start";
  string probe_end_annotation = method + "_end";

  if (fun != NULL) {
    // Need to first deactivate before activating. Otherwise the return from the generated stub will be written with a junk address in a mysterious manner.
    // Probably this will be fixed by atomic modify_probe_site implementation
    deactivateProbe(probe_start_annotation);
    deactivateProbe(probe_end_annotation);
    activateProbe(probe_start_annotation, fun);
    activateProbe(probe_end_annotation, fun);
  } else {
    deactivateProbe(probe_start_annotation);
    deactivateProbe(probe_end_annotation);
    activateProbe(probe_start_annotation, profiler_func);
    activateProbe(probe_end_annotation, profiler_func);
  }
}

void stop_profile(string method) {

  string probe_start_annotation = method + "_start";
  string probe_end_annotation = method + "_end";

  deactivateProbe(probe_start_annotation);
  deactivateProbe(probe_end_annotation);
}

void tokenize(const string& str,
                      vector<string>& tokens,
                      const string& delimiters = " ")
{
    // Skip delimiters at beginning.
    string::size_type lastPos = str.find_first_not_of(delimiters, 0);
    // Find first "non-delimiter".
    string::size_type pos     = str.find_first_of(delimiters, lastPos);

    while (string::npos != pos || string::npos != lastPos)
    {
        // Found a token, add it to the vector.
        tokens.push_back(str.substr(lastPos, pos - lastPos));
        // Skip delimiters.  Note the "not_of"
        lastPos = str.find_first_not_of(delimiters, pos);
        // Find next "non-delimiter"
        pos = str.find_first_of(delimiters, lastPos);
    }
}

void profile_all(void (*fun)()) {
	map<string, int>* activated_probes = new map<string, int>;

	for (auto iter = annotations.begin(); iter != annotations.end(); iter++) {
		string annotation = iter->first;

		vector<string> tokens;
		tokenize(annotation, tokens, "_");

		string func_name = tokens[0];

		map<string, int>::iterator it;
		it = activated_probes->find(func_name);

		if (it == activated_probes->end()) {
			start_profile(func_name, fun);
		}

		activated_probes->insert(make_pair(func_name,1));
	}

	delete activated_probes;
}

void turn_off_profiler() {
	map<string, int>* deactivated_probes = new map<string, int>;

	for (auto iter = annotations.begin(); iter != annotations.end(); iter++) {
		string annotation = iter->first;

		vector<string> tokens;
		tokenize(annotation, tokens, "_");

		string func_name = tokens[0];

		map<string, int>::iterator it;
		it = deactivated_probes->find(func_name);

		if (it == deactivated_probes->end()) {
			stop_profile(func_name);
		}

		deactivated_probes->insert(make_pair(func_name,1));
	}

	delete deactivated_probes;
}

/*uint64_t gettid() {
  pthread_t ptid = pthread_self();
  uint64_t threadId = 0;
  memcpy(&threadId, &ptid, std::min(sizeof(threadId), sizeof(ptid)));
  return threadId;
  }*/

static pthread_key_t key;
static pthread_once_t tls_init_flag = PTHREAD_ONCE_INIT;

void placement_delete(void *t) {
	function_stats* f_stats = (function_stats*)t;
	for (auto iter = f_stats->begin(); iter != f_stats->end(); iter++) {
		prof_data* data = iter->second;
		if (data != NULL) {
			// printf("Data->count : %lu\n", data->count);
			free(data);
		}
	}


	// printf("Delete called..\n");
	// ((Statistics*)t)->~Statistics();
}

void create_key() {
	pthread_key_create(&key, placement_delete);
}

// int counter = 0;

void profiler_func() {

	__thread static bool allocated;
	// __thread static function_stats stats;
	uint64_t addr;
	uint64_t offset = 2;

	// Gets [%rbp + 16] to addr. This is a hacky way to get the function parameter (annotation string) pushed to the stack
	// before the call to this method. Ideally this should be accessible by declaring an explicit method paramter according
	// x86 calling conventions AFAIK. But it fails to work that way hence we do the inline assembly to get it.
	// Fix this elegantly with a method parameter should be a TODO
	asm (
		"movq (%%rbp, %1, 8), %0\n\t"
		: "=r"(addr)
        : "c" (offset)
	);

	char* annotation = (char*)addr;

	if (!allocated) {
		function_stats* stats = new function_stats;
		allocated = true;

		pthread_once(&tls_init_flag, create_key);
		pthread_setspecific(key, stats);
	}

	// function_stats& func_stats = *((function_stats*) &stats);
	// function_stats func_stats = stats.f_stats;

	prof_data* data;
	function_stats* stats = (function_stats*)pthread_getspecific(key);

	char* func_name;
	char* tok;
	if (annotation != NULL) {
		func_name = strtok_r(annotation, "_", &tok);
	} else {
		return;
	}

	if (stats->find(func_name) == stats->end()) {
		data = (prof_data*)malloc(sizeof(prof_data));
		data->start = -1;
		data->min = 0;
		data->max = 0;
		data->sum = 0;
		data->count = 0;

		stats->insert(make_pair(func_name, data));

		// printf("Initialing the map..\n");

		// printf("func stat value : %d\n", func_stats.find("a")->second->start);
		// printf("func stat is at : %p\n", &func_stats);
		// printf("data is at : %p\n", data);

	} else {
		data = stats->find(func_name)->second;
	}

	if (data->start == -1) {
		ticks time = getticks();
		data->start = time;
	} else {
		ticks time = getticks();
		ticks end = time;
		ticks elapsed = end - data->start;

		if (elapsed < data->min || data->min == 0) {
			data->min = elapsed;
		}

		if (elapsed > data->max) {
			data->max = elapsed;
		}

		data->sum = data->sum + elapsed;
		data->count += 1;

		data->start = -1;
	}

	// Merge to the global statistics table
	if (data->count == 1000) {
		prof_data* global_data;

		// Acquire the spin lock
		while (!(__sync_bool_compare_and_swap(&spin_lock, 0 , 1)));

		if (statistics.find(func_name) == statistics.end()) {
			global_data = (prof_data*)malloc(sizeof(prof_data));
			global_data->min = 0;
			global_data->max = 0;
			global_data->sum = 0;
			global_data->count = 0;

			statistics.insert(make_pair(func_name, global_data));
		} else {
			global_data = statistics.find(func_name)->second;
		}

		if (global_data->min > data->min || global_data->min == 0){
			global_data->min = data->min;
		}

		if (global_data->max < data->max) {
			global_data->max = data->max;
		}

		global_data->sum = global_data->sum + data->sum;
		global_data->count = global_data->count + data->count;

		// Release lock
		__sync_bool_compare_and_swap(&spin_lock, 1 , 0);

		if (global_data->count == 1000) {
			printf("\nFunction : %s\n", func_name);
			printf("Min : %lu\n", global_data->min);
			printf("Max : %lu\n", global_data->max);
			printf("Avg : %lu\n", global_data->sum / global_data->count);
		}
	}

  // printf("Inside profile function..\n");
  return;

}

// ticks func1_min = 0;
// ticks func1_max = 0;
// ticks func1_sum = 0;
// int func1_count = 0;
// int spin_lock_1 = 0;
//
//
// void func1(){
//   int x;
//   ticks start = getticks();
//   __notify_intrinsic((void*)"func1_start",(void*)&x);
//
//   // func2();
//
//   uint64_t i;
//   for (i=0; i < 1000; i++) {
//     srand(time(NULL));
//     int r = rand();
//
//     // func2();
//   }
//
//   __notify_intrinsic((void*)"func1_end",(void*)&x);
//   ticks end = getticks();
//   ticks elapsed = (end - start);
//
//   while (!(__sync_bool_compare_and_swap(&spin_lock_1, 0 , 1)));
//
//   if (elapsed < func1_min || func1_min == 0) {
//     func1_min = elapsed;
//   }
//
//   if (elapsed > func1_max) {
//     func1_max = elapsed;
//   }
//
//   func1_sum += elapsed;
//   func1_count++;
//
//   __sync_bool_compare_and_swap(&spin_lock_1, 1 , 0);
//
//   if (func1_count == 2000) {
//     printf("\nMin elapsed time (total : func + prof overhead) in func1 : %lu\n", func1_min);
//     printf("Max elapsed time (total : func + prof overhead) in func1 : %lu\n", func1_max);
//     printf("Avg elapsed time (total : func + prof overhead) in func1 : %lu\n", (func1_sum / func1_count));
//   }
// }
//
// ticks func2_min = 0;
// ticks func2_max = 0;
// ticks func2_sum = 0;
// int func2_count = 0;
// int spin_lock_2 = 0;
//
// void func2() {
//   int x;
//   ticks start = getticks();
//   __notify_intrinsic((void*)"func2_start",(void*)&x);
//
//   /*	uint64_t i;
//       for (i=0; i < 1000; i++) {
//       srand(time(NULL));
//       int r = rand();
//       }*/
//
//   __notify_intrinsic((void*)"func2_end",(void*)&x);
//   ticks end = getticks();
//   ticks elapsed = (end - start);
//
//   while (!(__sync_bool_compare_and_swap(&spin_lock_2, 0 , 1)));
//
//   if (elapsed < func2_min || func2_min == 0) {
//     func2_min = elapsed;
//   }
//
//   if (elapsed > func2_max) {
//     func2_max = elapsed;
//   }
//
//   func2_sum += elapsed;
//   func2_count++;
//
//   __sync_bool_compare_and_swap(&spin_lock_2, 1 , 0);
//
//   if (func2_count == 2000) {
//     printf("\nMin elapsed time (total : func + prof overhead) in func2 : %lu\n", func2_min);
//     printf("Max elapsed time (total : func + prof overhead) in func2 : %lu\n", func2_max);
//     printf("Avg elapsed time (total : func + prof overhead) in func2 : %lu\n", (func2_sum / func2_count));
//   }
// }
//
// void* func1_loop(void* tid) {
//
//   // start_profile("func1_start", NULL);
//   // start_profile("func1_end", NULL);
//   int i;
// #pragma noinline recursive
//   for (i=0; i < 1000; i++) {
//     func1();
//     func2();
//   }
//
//   // stop_profile("func1_start");
//   // stop_profile("func1_end");
//
//   return NULL;
// }
//
// void* func2_loop(void* tid) {
//
//   // start_profile("func2_start", NULL);
//   // start_profile("func2_end", NULL);
//
//   int i;
//   for (i=0; i < 1000; i++) {
//     func2();
//     func1();
//   }
//
//   // stop_profile("func2_start");
//   // stop_profile("func2_end");
//
//   return NULL;
// }
//
// int main() {
//
//   initZCAService();
//   pthread_t func1_t;
//   int rc = pthread_create(&func1_t, NULL, func1_loop, (void*)"Function_1");
//   if (rc){
//     printf("ERROR; return code from pthread_create() is %d\n", rc);
//     exit(-1);
//   }
//
//   if(pthread_join(func1_t, NULL)) {
//     printf("Error joining thread\n");
//     exit(-1);
//   }
//
//   pthread_t func2_t;
//   rc = pthread_create(&func2_t, NULL, func2_loop, (void*)"Function_2");
//   if (rc){
//     printf("ERROR; return code from pthread_create() is %d\n", rc);
//     exit(-1);
//   }
//
//   if(pthread_join(func1_t, NULL)) {
//     printf("Error joining thread\n");
//     exit(-1);
//   }
// }
//
//
