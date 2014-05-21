
#include "zca-toggle.hpp"
#include "elf-provider.h"
#include "profiler_types.hpp"
#include "dynaprof.h"
#include <stdio.h>
#include <pthread.h>
#include <time.h>
#include <stdlib.h>
#include <unistd.h>
#include <string>
#include <string.h>
#include <vector>
#include "cycle.h"

using namespace std;

global_stats statistics;
volatile int spin_lock = 0;
volatile int spin_lock_0 = 0;

Profiler* prof;
timestamps* deactivations;

void func2();

// Check this out later
//  http://stackoverflow.com/questions/2053029/how-exactly-does-attribute-constructor-work

void* probe_monitor(void* param) {
	sleep(1);

	return NULL;
}

__attribute__((constructor))
void initialize(void) {

	// pthread create
	pthread_t tid;
	pthread_create(&tid, NULL, probe_monitor, (void*)NULL);

}

// __attribute__((destructor)) - This doesn't seem to work properly with our heap data being tampered with when this gets called
void cleanup(void) {

	typedef std::map<std::string, prof_data*>::iterator it_type;
	int counter = 0;
  FILE *out_file = fopen("prof.out", "w");
	for(auto iterator = statistics.begin(); iterator != statistics.end(); iterator++) {
		counter++;
		fprintf(out_file, "\nFunction : %s\n", iterator->first);
		printf("\nFunction : %s\n", iterator->first);

		prof_data* data = iterator->second;
		fprintf(out_file, "Count : %d\n", data->count);
		fprintf(out_file, "Min : %lu\n", data->min);
		fprintf(out_file, "Max : %lu\n", data->max);
		fprintf(out_file, "Avg : %lu\n", data->sum / data->count);
	}

  fclose(out_file);

	// Deallocate all the allocated stuff here

}

void activate_method_profiling(char* method, void (*fun)()) {
	prof->start_profile(string(method), fun);
}

void deactivate_method_profiling(char* method) {
	prof->stop_profile(string(method));
}

void start_profiler() {
	initZCAService();
	prof = new Basic_Profiler;
	prof->profile_all(NULL);

	atexit(cleanup); // This seems to be a viable alternative to the destructor
	// printf("Instrumentation done..\n");
}

void stop_profiler() {
	delete prof;
}

void Profiler::start_profile(string method, void (*fun)() ) {

	string probe_start_annotation = method + ":start";
	string probe_end_annotation = method + ":end";

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

		activateProbe(probe_start_annotation, (this->fun));
		activateProbe(probe_end_annotation, (this->fun));
	}
}

void Profiler::stop_profile(string method) {

	string probe_start_annotation = method + ":start";
	string probe_end_annotation = method + ":end";

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
	string::size_type pos = str.find_first_of(delimiters, lastPos);

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

void Profiler::profile_all(void (*fun)()) {
	map<string, int>* activated_probes = new map<string, int>;

	for (auto iter = annotations.begin(); iter != annotations.end(); iter++) {
		string annotation = iter->first;

		vector<string> tokens;
		tokenize(annotation, tokens, ":");

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

void Profiler::turn_off_profiler() {
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

void Basic_Profiler::set_profiler_function() {
	this->fun = basic_profiler_func;
}

void basic_profiler_func() {

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

	// char test = *(NULL);

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
		char *temp = strdup(annotation);
		func_name = strtok_r(temp, ":", &tok);
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
		return;
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
	if (data->count >= 1) {
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
		data->count = 0;
		data->sum = 0;

		if (global_data->count > DEACTIVATION_THRESHOLD) {
      deactivate_method_profiling(func_name);
			fprintf(stderr, "\n***************************** Deactivating function : %s\n ***************************", func_name);
		}

		// Release lock
		__sync_bool_compare_and_swap(&spin_lock, 1 , 0);

/*		if (global_data->count >= 1) {
			fprintf(stderr, "\nFunction : %s\n", func_name);
			fprintf(stderr, "Count : %lu\n", global_data->count);
			fprintf(stderr, "Min : %lu\n", global_data->min);
			fprintf(stderr, "Max : %lu\n", global_data->max);
			fprintf(stderr, "Avg : %lu\n", global_data->sum / global_data->count);
		}*/
	}

	// printf("Inside profile function..\n");
	return;

}


